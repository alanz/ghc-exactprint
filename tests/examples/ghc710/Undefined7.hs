{-# LANGUAGE TemplateHaskell, QuasiQuotes, StandaloneDeriving, DeriveDataTypeable #-}

module Test where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.Map as M
import Data.Generics

import Data.Binary.ISO8583
import Data.Binary.ISO8583.TH

[binary|
  Message
  2 pan embedded 2
  4 amount int 12
  11 stan int 6
  43 termAddress TermAddress 222
|]

deriving instance Eq Message
deriving instance Show Message

data TermAddress = TermAddress {
      tOwner :: B.ByteString,
      tCity :: B.ByteString,
      tOther :: L.ByteString }
  deriving (Eq, Show, Typeable)

instance Binary TermAddress where
  -- NB: this implementation is smth odd and usable only for this testcase.
  get =
    TermAddress
      <$> B.filter (/= 0x20) `fmap` getByteString 30
      <*> B.filter (/= 0x20) `fmap` getByteString 30
      <*> L.filter (/= 0x20) `fmap` getRemainingLazyByteString

  put (TermAddress owner city other) = do
    putByteStringPad 30 owner
    putByteStringPad 30 city
    putLazyByteStringPad 162 other

instance Binary Message where
  get = do
    m <- getBitmap getMessage
    return $ constructMessage m

  put msg = do
    putBitmap' (putMessage msg)

testMsg :: Message
testMsg = Message {
  pan = Just $ toBS "12345678",
  amount = Just $ 100500,
  stan = Just $ 123456,
  termAddress = Just $ TermAddress {
                  tOwner = toBS "TestBank",
                  tCity = toBS "Magnitogorsk",
                  tOther = L.empty }
}

test :: IO ()
test = do
  let bstr = encode testMsg
      msg = decode bstr
  if msg /= testMsg
    then fail $ "Encode/decode mismatch:\n" ++
           show testMsg ++ "\n /= \n" ++
           show msg
    else putStrLn "passed."

