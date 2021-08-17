-- Copyright 2002 Warrick Gray
-- Copyright 2001,2002 Peter Thiemann
-- Copyright 2003-2006 Bjorn Bringert
-- Copyright 2009 Henning Thielemann
module Network.MoHWS.HTTP.Header (
    Group, group, ungroup,
    setGroup,
    getGroup,
    list,
    modifyMany,
    T, Hdrs.Header(..), make,
    getName, getValue, name, value,
    Name, Hdrs.HeaderName(..),
    HasHeaders,
    -- * Header parsing
    pGroup, makeName,
    -- * Header manipulation
    insert,
    insertIfMissing,
    replace, insertMany,
    lookupMany, lookup,
    -- * Constructing headers
    makeContentLength,
    makeContentType,
    makeLocation,
    makeLastModified,
    TransferCoding(..),
    makeTransferCoding,
    -- * Getting values of specific headers
    getContentType,
    getContentLength,
--    getHost,
   ) where

import qualified Network.HTTP.Headers as Hdrs
import Network.HTTP.Headers (HasHeaders, )

import Network.MoHWS.ParserUtility
import Network.MoHWS.Utility

-- import Network.Socket (HostName, )
import Network.URI (URI, )

import Control.Monad (liftM, )
import Data.Char (toLower, )
import Data.Map (Map, )
import qualified Data.Map as Map hiding (Map)
import System.Time (ClockTime, toUTCTime, )
import Text.ParserCombinators.Parsec
          (Parser, char, skipMany, many, )

import qualified Data.Accessor.Basic as Accessor

import Prelude hiding (lookup, )


-- * Header

type T    = Hdrs.Header
type Name = Hdrs.HeaderName


make :: Name -> String -> T
make = Hdrs.Header

getName :: T -> Name
getName (Hdrs.Header n _v) = n

getValue :: T -> String
getValue (Hdrs.Header _n v) = v

name :: Accessor.T T Name
name =
   Accessor.fromSetGet
      (\n (Hdrs.Header _ v) -> Hdrs.Header n v)
      getName

value :: Accessor.T T String
value =
   Accessor.fromSetGet
      (\v (Hdrs.Header n _) -> Hdrs.Header n v)
      getValue


-- Translation between header names and values,
nameList :: [ (String, Name) ]
nameList =
   ("Cache-Control"        , Hdrs.HdrCacheControl      ) :
   ("Connection"           , Hdrs.HdrConnection        ) :
   ("Date"                 , Hdrs.HdrDate              ) :
   ("Pragma"               , Hdrs.HdrPragma            ) :
--   ("Trailer"              , Hdrs.HdrTrailer           ) :

   ("Transfer-Encoding"    , Hdrs.HdrTransferEncoding  ) :
   ("Upgrade"              , Hdrs.HdrUpgrade           ) :
   ("Via"                  , Hdrs.HdrVia               ) :
   ("Accept"               , Hdrs.HdrAccept            ) :
   ("Accept-Charset"       , Hdrs.HdrAcceptCharset     ) :
   ("Accept-Encoding"      , Hdrs.HdrAcceptEncoding    ) :
   ("Accept-Language"      , Hdrs.HdrAcceptLanguage    ) :
   ("Authorization"        , Hdrs.HdrAuthorization     ) :
   ("From"                 , Hdrs.HdrFrom              ) :
   ("Host"                 , Hdrs.HdrHost              ) :
   ("If-Modified-Since"    , Hdrs.HdrIfModifiedSince   ) :
   ("If-Match"             , Hdrs.HdrIfMatch           ) :
   ("If-None-Match"        , Hdrs.HdrIfNoneMatch       ) :
   ("If-Range"             , Hdrs.HdrIfRange           ) :
   ("If-Unmodified-Since"  , Hdrs.HdrIfUnmodifiedSince ) :
   ("Max-Forwards"         , Hdrs.HdrMaxForwards       ) :
   ("Proxy-Authorization"  , Hdrs.HdrProxyAuthorization) :
   ("Range"                , Hdrs.HdrRange             ) :
   ("Referer"              , Hdrs.HdrReferer           ) :
--   ("TE"                   , Hdrs.HdrTE                ) :
   ("User-Agent"           , Hdrs.HdrUserAgent         ) :
   ("Age"                  , Hdrs.HdrAge               ) :
   ("Location"             , Hdrs.HdrLocation          ) :
   ("Proxy-Authenticate"   , Hdrs.HdrProxyAuthenticate ) :
   ("Public"               , Hdrs.HdrPublic            ) :
   ("Retry-After"          , Hdrs.HdrRetryAfter        ) :
   ("Server"               , Hdrs.HdrServer            ) :
   ("Vary"                 , Hdrs.HdrVary              ) :
   ("Warning"              , Hdrs.HdrWarning           ) :
   ("WWW-Authenticate"     , Hdrs.HdrWWWAuthenticate   ) :
   ("Allow"                , Hdrs.HdrAllow             ) :
   ("Content-Base"         , Hdrs.HdrContentBase       ) :
   ("Content-Encoding"     , Hdrs.HdrContentEncoding   ) :
   ("Content-Language"     , Hdrs.HdrContentLanguage   ) :
   ("Content-Length"       , Hdrs.HdrContentLength     ) :
   ("Content-Location"     , Hdrs.HdrContentLocation   ) :
   ("Content-MD5"          , Hdrs.HdrContentMD5        ) :
   ("Content-Range"        , Hdrs.HdrContentRange      ) :
   ("Content-Type"         , Hdrs.HdrContentType       ) :
   ("ETag"                 , Hdrs.HdrETag              ) :
   ("Expires"              , Hdrs.HdrExpires           ) :
   ("Last-Modified"        , Hdrs.HdrLastModified      ) :
   ("Set-Cookie"           , Hdrs.HdrSetCookie         ) :
   ("Cookie"               , Hdrs.HdrCookie            ) :
   ("Expect"               , Hdrs.HdrExpect            ) :
   []

toNameMap :: Map String Name
toNameMap =
   Map.fromList [(map toLower x, y) | (x,y) <- nameList]

{-
fromHeaderNameMap :: Map Name String
fromHeaderNameMap = Map.fromList [(y,x) | (x,y) <- headerNames]
-}

makeName :: String -> Name
makeName s =
   Map.findWithDefault (Hdrs.HdrCustom s) (map toLower s) toNameMap


-- * Header group

newtype Group = Group { ungroup :: [T] }

group :: [T] -> Group
group = Group

instance Show Group where
   showsPrec _ =
      foldr (.) id . map shows . ungroup
--      foldr (.) id . map (\x -> shows x . showString crLf) . unGroup

instance HasHeaders Group where
   getHeaders = ungroup
   setHeaders _ = group


getGroup :: HasHeaders x => x -> Group
getGroup = group . Hdrs.getHeaders

setGroup :: HasHeaders x => x -> Group -> x
setGroup x = Hdrs.setHeaders x . ungroup

list :: HasHeaders x => x -> [T]
list = Hdrs.getHeaders

modifyMany :: HasHeaders x => ([T] -> [T]) -> x -> x
modifyMany f x = Hdrs.setHeaders x $ f $ Hdrs.getHeaders x


-- * Header manipulation functions

-- Header manipulation functions
insert, replace, insertIfMissing :: HasHeaders a =>
   Name -> String -> a -> a
insert = Hdrs.insertHeader
insertIfMissing = Hdrs.insertHeaderIfMissing
replace = Hdrs.replaceHeader

insertMany :: HasHeaders a => [T] -> a -> a
insertMany = Hdrs.insertHeaders


lookupMany :: HasHeaders a => Name -> a -> [String]
lookupMany searchName x =
   [ v | Hdrs.Header n v <- list x, searchName == n ]

lookup :: HasHeaders a => Name -> a -> Maybe String
lookup n = Hdrs.lookupHeader n . list
-- lookup n x = listToMaybe $ lookupMany n x


-- * Constructing specific headers

makeContentLength :: Integer -> T
makeContentLength i = Hdrs.Header Hdrs.HdrContentLength (show i)

makeContentType :: String -> T
makeContentType t = Hdrs.Header Hdrs.HdrContentType t

makeLocation :: URI -> T
makeLocation t = Hdrs.Header Hdrs.HdrLocation $ show t

makeLastModified :: ClockTime -> T
makeLastModified t =
   Hdrs.Header Hdrs.HdrLastModified (formatTimeSensibly (toUTCTime t))

makeTransferCoding :: TransferCoding -> T
makeTransferCoding te = Hdrs.Header Hdrs.HdrTransferEncoding (transferCodingStr te)

data TransferCoding
  = ChunkedTransferCoding
  | GzipTransferCoding
  | CompressTransferCoding
  | DeflateTransferCoding
  deriving Eq

transferCodingStr :: TransferCoding -> String
transferCodingStr ChunkedTransferCoding  = "chunked"
transferCodingStr GzipTransferCoding     = "gzip"
transferCodingStr CompressTransferCoding = "compress"
transferCodingStr DeflateTransferCoding  = "deflate"

-- validTransferCoding :: [TransferCoding] -> Bool
-- validTransferCoding codings
--   | null codings
--     || last codings == ChunkedTransferCoding
--        && ChunkedTransferCoding `notElem` init codings = True
--   | otherwise = False
;

-- * Values of specific headers

getContentType :: HasHeaders a => a -> Maybe String
getContentType x = lookup Hdrs.HdrContentType x

getContentLength :: HasHeaders a => a -> Maybe Integer
getContentLength x = lookup Hdrs.HdrContentLength x >>= readM

{-
getHost :: HasHeaders a => a -> Maybe (HostName, Maybe Int)
getHost x = lookup Hdrs.HdrHost x >>= parseHost
-}


-- * Parsing

pGroup :: Parser Group
pGroup = liftM group $ many pHeader

pHeader :: Parser T
pHeader =
    do n <- pToken
       _ <- char ':'
       skipMany pWS1
       line <- lineString
       _ <- pCRLF
       extraLines <- many extraFieldLine
       return $ Hdrs.Header (makeName n) (concat (line:extraLines))

extraFieldLine :: Parser String
extraFieldLine =
    do sp <- pWS1
       line <- lineString
       _ <- pCRLF
       return (sp:line)

{-
parseHost :: String -> Maybe (HostName, Maybe Int)
parseHost s =
    let (host,prt) = break (==':') s
    in  case prt of
           ""       -> Just (host, Nothing)
           ':':port -> readM port >>= \p -> Just (host, Just p)
           _        -> Nothing
-}

