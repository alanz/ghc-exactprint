{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.TigHTTP.Papillon (
    ContentType(..), Type(..), Subtype(..), Parameter(..), Charset(..),
        parseContentType, showContentType,
) where

import Data.Char
import Text.Papillon

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString as BS

import Network.TigHTTP.Token

data ContentType = ContentType Type Subtype [Parameter]
    deriving (Show, Eq)

parseContentType :: BS.ByteString -> ContentType
parseContentType ct = case runError . contentType $ parse ct of
    Left _ -> error "parseContentType"
    Right (r, _) -> r

showContentType :: ContentType -> BS.ByteString
showContentType (ContentType t st ps) = showType t
    `BS.append` "/"
    `BS.append` showSubtype st
    `BS.append` showParameters ps

data Type
    = Text
    | TypeRaw BS.ByteString
    deriving (Show, Eq)

mkType :: BS.ByteString -> Type
mkType "text" = Text
mkType t = TypeRaw t

showType :: Type -> BS.ByteString
showType Text = "text"
showType (TypeRaw t) = t

data Subtype
    = Plain
    | Html
    | Css
    | SubtypeRaw BS.ByteString
    deriving (Show, Eq)

mkSubtype :: BS.ByteString -> Subtype
mkSubtype "html" = Html
mkSubtype "plain" = Plain
mkSubtype "css" = Css
mkSubtype s = SubtypeRaw s

showSubtype :: Subtype -> BS.ByteString
showSubtype Plain = "plain"
showSubtype Html = "html"
showSubtype Css = "css"
showSubtype (SubtypeRaw s) = s

data Parameter
    = Charset Charset
    | ParameterRaw BS.ByteString BS.ByteString
    deriving (Show, Eq)

mkParameter :: BS.ByteString -> BS.ByteString -> Parameter
mkParameter "charset" "UTF-8" = Charset Utf8
mkParameter "charset" v = Charset $ CharsetRaw v
mkParameter a v = ParameterRaw a v

showParameters :: [Parameter] -> BS.ByteString
showParameters [] = ""
showParameters (Charset v : ps) = "; " `BS.append` "charset"
    `BS.append` "=" `BS.append` showCharset v `BS.append` showParameters ps
showParameters (ParameterRaw a v : ps) = "; " `BS.append` a
    `BS.append` "=" `BS.append` v `BS.append` showParameters ps

data Charset
    = Utf8
    | CharsetRaw BS.ByteString
    deriving (Show, Eq)

showCharset :: Charset -> BS.ByteString
showCharset Utf8 = "UTF-8"
showCharset (CharsetRaw cs) = cs

bsconcat :: [ByteString] -> ByteString
bsconcat = BS.concat

[papillon|

source: ByteString

contentType :: ContentType
    = c:token '/' sc:token ps:(';' ' '* p:parameter { p })*
    { ContentType (mkType c) (mkSubtype sc) ps }

token :: ByteString
    = t:<isTokenChar>+          { pack t }

quotedString :: ByteString
    = '"' t:(qt:qdtext { qt } / qp:quotedPair { pack [qp] })* '"'
                        { bsconcat t }

quotedPair :: Char
    = '\\' c:<isAscii>          { c }

crlf :: () = '\r' '\n'

lws :: () = _:crlf _:(' ' / '\t')+

-- text :: ByteString
--  = ts:(cs:<isTextChar>+ { cs } / _:lws { " " })+     { pack $ concat ts }

qdtext :: ByteString
    = ts:(cs:<isQdtextChar>+ { cs } / _:lws { " " })+   { pack $ concat ts }

parameter :: Parameter
    = a:attribute '=' v:value               { mkParameter a v }

attribute :: ByteString = t:token               { t }

value :: ByteString
    = t:token                       { t }
    / qs:quotedString                   { qs }

|]
