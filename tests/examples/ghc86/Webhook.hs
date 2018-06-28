{-|
Module      : Servant.GitHub.Webhook
Description : Easily write safe GitHub webhook handlers with Servant
Copyright   : (c) Jacob Thomas Errington, 2016
License     : MIT
Maintainer  : servant-github-webhook@mail.jerrington.me
Stability   : experimental

The GitHub webhook machinery will attach three headers to the HTTP requests
that it fires: @X-Github-Event@, @X-Hub-Signature@, and @X-Github-Delivery@.
The former two headers correspond with the 'GitHubEvent' and
'GitHubSignedReqBody''' routing combinators. This library ignores the
@X-Github-Delivery@ header; if you would like to access its value, then use the
builtin 'Header' combinator from Servant.

Usage of the library is straightforward: protect routes with the 'GitHubEvent'
combinator to ensure that the route is only reached for specific
'RepoWebhookEvent's, and replace any 'ReqBody' combinators you would write
under that route with 'GitHubSignedReqBody'. It is advised to always include a
'GitHubSignedReqBody''', as this is the only way you can be sure that it is
GitHub who is sending the request, and not a malicious user. If you don't care
about the request body, then simply use Aeson\'s 'Object' type as the
deserialization target -- @GitHubSignedReqBody' key '[JSON] Object@ -- and
ignore the @Object@ in the handler.

The 'GitHubSignedReqBody''' combinator makes use of the Servant 'Context' in
order to extract the signing key. This is the same key that must be entered in
the configuration of the webhook on GitHub. See 'GitHubKey'' for more details.

In order to support multiple keys on a per-route basis, the basic combinator
@GitHubSignedReqBody''@ takes as a type parameter as a key index. To use this,
create a datatype, e.g. @KeyIndex@ whose constructors identify the different
keys you will be using. Generally, this means one constructor per repository.
Use the @DataKinds@ extension to promote this datatype to a kind, and write an
instance of 'Reflect' for each promoted constructor of your datatype. Finally,
create a 'Context' containing 'GitHubKey'' whose wrapped function's domain is
the datatype you've built up. Thus, your function can determine which key to
retrieve.
-}

{-# LANGUAGE CPP #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- GHC 8 seems to have improved its decidability check for type family
-- instances and class instances. In particular, without UndecidableInstances
-- enabled, the Demote' instance for lists, which we need, will not compile.
-- Similarly, the Reflect instance for Symbol, which just requires KnownSymbol,
-- won't compile on GHC < 8 because the instance head is no smaller than the
-- instance head.
#if __GLASGOW_HASKELL__ < 800
{-# LANGUAGE UndecidableInstances #-}
#endif

module Servant.GitHub.Webhook
( -- * Servant combinators
  GitHubSignedReqBody''
, GitHubSignedReqBody'
, GitHubSignedReqBody
, GitHubEvent

  -- ** Security
, GitHubKey'(..)
, GitHubKey
, gitHubKey

  -- * Reexports
  --
  -- | We reexport a few datatypes that are typically needed to use the
  -- library.
, RepoWebhookEvent(..)
, KProxy(..)

  -- * Implementation details

  -- ** Type-level programming machinery
, Demote
, Demote'
, Reflect(..)

  -- ** Stringy stuff
, parseHeaderMaybe
, matchEvent

  -- * Examples
  --
  -- $example1
  --
  -- $example2
) where

import Control.Monad.IO.Class ( liftIO )
import Data.Aeson ( decode', encode )
import qualified Data.ByteString as BS
import Data.ByteString.Lazy ( fromStrict, toStrict )
import qualified Data.ByteString.Base16 as B16
import Data.HMAC ( hmac_sha1 )
import Data.List ( intercalate )
import Data.Maybe ( catMaybes, fromMaybe )
import Data.Monoid ( (<>) )
import Data.Proxy
import Data.String.Conversions ( cs )
import qualified Data.Text.Encoding as E
import GHC.TypeLits
import GitHub.Data.Webhooks
import Network.HTTP.Types hiding (Header, ResponseHeaders)
import Network.Wai ( requestHeaders, strictRequestBody )
import Servant
import Servant.API.ContentTypes ( AllCTUnrender(..) )
import Servant.Server.Internal


-- | A clone of Servant's 'ReqBody' combinator, except that it will also
-- verify the signature provided by GitHub in the @X-Hub-Signature@ header by
-- computing the SHA1 HMAC of the request body and comparing.
--
-- The use of this combinator will require that the router context contain an
-- appropriate 'GitHubKey'' entry. Specifically, the type parameter of
-- 'GitHubKey'' must correspond with @Demote k@ where @k@ is the kind of the
-- index @key@ used here. Consequently, it will be necessary to use
-- 'serveWithContext' instead of 'serve'.
--
-- Other routes are not tried upon the failure of this combinator, and a 401
-- response is generated.
--
-- Use of this datatype directly is discouraged, since the choice of the index
-- @key@ determines its kind @k@ and hence @proxy@, which is . Instead, use
-- 'GitHubSignedReqBody'', which computes the @proxy@ argument given just
-- @key@. The proxy argument is necessary to avoid @UndecidableInstances@ for
-- the implementation of the 'HasServer' instance for the datatype.
data GitHubSignedReqBody''
  (proxy :: KProxy k)
  (key :: k)
  (list :: [Type])
  (result :: Type) where

-- | Convenient synonym for 'GitHubSignedReqBody''' that computes its first
-- type argument given just the second one.
--
-- Use this type synonym if you are creating a webhook server to handle
-- webhooks from multiple repositories, with different secret keys.
type GitHubSignedReqBody' (key :: k)
  = GitHubSignedReqBody'' ('KProxy :: KProxy k) key

-- | A convenient alias for a trivial key index.
--
-- USe this type synonym if you are creating a webhook server to handle only
-- webhooks from a single repository, or for mutliple repositories using the
-- same secret key.
type GitHubSignedReqBody = GitHubSignedReqBody' '()

-- | A routing combinator that succeeds only for a webhook request that matches
-- one of the given 'RepoWebhookEvent' given in the type-level list @events@.
--
-- If the list contains 'WebhookWildcardEvent', then all events will be
-- matched.
--
-- The combinator will require that its associated handler take a
-- 'RepoWebhookEvent' parameter, and the matched event will be passed to the
-- handler. This allows the handler to determine which event triggered it from
-- the list.
--
-- Other routes are tried if there is a mismatch.
data GitHubEvent (events :: [RepoWebhookEvent]) where

