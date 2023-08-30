{-# language BangPatterns #-}
{-# language BinaryLiterals #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}

module Json.Token
  ( Token(..)
  , TokenException(..)
  , decode
  ) where

import Control.Monad.ST (ST,runST)
import Data.Bits ((.&.),(.|.),unsafeShiftR)
import Data.Builder.ST (Builder)
import Data.Bytes.Parser (Parser)
import Data.Bytes.Types (Bytes(..))
import Data.Char (ord)
import Data.Primitive (MutableByteArray,ByteArray)
import Data.Primitive (SmallArray)
import Data.Text.Short (ShortText)
import Data.Word (Word8,Word16)
import GHC.Exts (Int(I#),Char(C#))
import GHC.Exts (word2Int#,chr#,gtWord#,ltWord#)
import GHC.Word (Word16(W16#),Word8(W8#))
import Data.Number.Scientific (Scientific)

import qualified Data.ByteString.Short.Internal as BSS
import qualified Data.Bytes.Parser as P
import qualified Data.Bytes.Parser.Utf8 as Utf8
import qualified Data.Bytes.Parser.Latin as Latin
import qualified Data.Bytes.Parser.Unsafe as Unsafe
import qualified Data.Builder.ST as B
import qualified Data.Chunks as C
import qualified Data.Primitive as PM
import qualified Data.Text.Short.Unsafe as TS
import qualified Data.Number.Scientific as SCI

-- | A token in a JSON document.
data Token
  = LeftBrace
  | RightBrace
  | LeftBracket
  | RightBracket
  | Comma
  | Colon
  | BooleanTrue
  | BooleanFalse
  | Null
  | String {-# UNPACK #-} !ShortText
  | Number {-# UNPACK #-} !Scientific
  deriving stock (Eq,Show)

-- | An exception encountered while tokenizing a JSON document.
data TokenException
  = InvalidNumber
  | InvalidLeader
  | ExpectedTrue
  | ExpectedFalse
  | ExpectedNull
  | LeadingZero
  | InvalidEscapeSequence
  | IncompleteString
  | IncompleteEscapeSequence
  deriving stock (Eq,Show)

isSpace :: Word8 -> Bool
isSpace w =
     w == c2w ' '
  || w == c2w '\t'
  || w == c2w '\r'
  || w == c2w '\n'

-- | Decode a sequence as JSON tokens. This allows token
-- sequences that would be rejected by the ABNF given in
-- <https://tools.ietf.org/html/rfc7159 RFC 7159>:
--
-- >>> decode (Bytes.fromAsciiString "[ , true }")
-- Right [ LeftBracket, Comma, BooleanTrue, RightBrace ]
--
-- It is up to the user to reject such malformed JSON when
-- they parse the token sequence. More surprisingly, this
-- tokenizer accepts some unnatural juxtapositions of token
-- sequences without whitespace. For example:
--
-- >>> decode (Bytes.fromAsciiString "55truefalse")
-- Right [ Number 55, BooleanTrue, BooleanFalse ]
-- >>> decode (Bytes.fromAsciiString "null\"hello\"")
-- Right [ Null, String "hello" ]
--
-- Acceptance of such samples simplifies the implementation
-- of this tokenizer. These unnatural juxtapositions always
-- result in token sequences that should be rejected anyway in
-- the subsequent parsing done by the user. Consequently, their
-- acceptance is not considered harmful.
decode :: Bytes -> Either TokenException (SmallArray Token)
decode !bs = runST $ do
  !b <- B.new
  P.parseBytesEffectfully (P.skipWhile isSpace *> manyTokens b) bs >>= \case
    P.Failure err -> pure (Left err)
    -- Since manyTokens only completes once the end of the input
    -- has been reached, we do not need to check the length here.
    P.Success (P.Slice _ _ cs) -> pure (Right cs)

manyTokens ::
     Builder s Token
  -> Parser TokenException s (SmallArray Token)
manyTokens !b0 = do
  t <- oneToken
  !b1 <- P.effect (B.push t b0)
  P.skipWhile isSpace
  done <- P.isEndOfInput
  if done
    then P.effect $ do
      cs <- B.freeze b1
      pure $! C.concat cs
    else manyTokens b1

-- TODO: oneToken is only called in contexts where the initial
-- call to Latin.any cannot fail. Consider refactoring to make
-- this more explicit.
oneToken :: Parser TokenException s Token
oneToken = Latin.any InvalidLeader >>= \case
  '{' -> pure LeftBrace
  '}' -> pure RightBrace
  '[' -> pure LeftBracket
  ']' -> pure RightBracket
  ',' -> pure Comma
  ':' -> pure Colon
  't' -> do
    Latin.char3 ExpectedTrue 'r' 'u' 'e'
    pure BooleanTrue
  'f' -> do
    Latin.char4 ExpectedFalse 'a' 'l' 's' 'e'
    pure BooleanFalse
  'n' -> do
    Latin.char3 ExpectedNull 'u' 'l' 'l'
    pure Null
  '"' -> do
    start <- Unsafe.cursor
    string start
  '-' -> fmap Number (SCI.parserNegatedUtf8Bytes InvalidNumber)
  '0' -> Latin.trySatisfy (\c -> c >= '0' && c <= '9') >>= \case
    True -> P.fail LeadingZero
    False -> fmap Number (SCI.parserTrailingUtf8Bytes InvalidNumber 0)
  c | c >= '1' && c <= '9' ->
        fmap Number (SCI.parserTrailingUtf8Bytes InvalidNumber (ord c - 48))
  _ -> P.fail InvalidLeader

copyAndEscape :: Int -> Parser TokenException s Token
copyAndEscape !maxLen = do
  !dst <- P.effect (PM.newByteArray maxLen)
  let go !ix = Utf8.any# IncompleteString `P.bindFromCharToLifted` \c -> case c of
        '\\'# -> Latin.any IncompleteEscapeSequence >>= \case
          '"' -> do
            P.effect (PM.writeByteArray dst ix (c2w '"'))
            go (ix + 1)
          '\\' -> do
            P.effect (PM.writeByteArray dst ix (c2w '\\'))
            go (ix + 1)
          't' -> do
            P.effect (PM.writeByteArray dst ix (c2w '\t'))
            go (ix + 1)
          'n' -> do
            P.effect (PM.writeByteArray dst ix (c2w '\n'))
            go (ix + 1)
          'r' -> do
            P.effect (PM.writeByteArray dst ix (c2w '\r'))
            go (ix + 1)
          '/' -> do
            P.effect (PM.writeByteArray dst ix (c2w '/'))
            go (ix + 1)
          'b' -> do
            P.effect (PM.writeByteArray dst ix (c2w '\b'))
            go (ix + 1)
          'f' -> do
            P.effect (PM.writeByteArray dst ix (c2w '\f'))
            go (ix + 1)
          'u' -> do
            w <- Latin.hexFixedWord16 InvalidEscapeSequence
            if w >= 0xD800 && w < 0xDFFF
              then go =<< P.effect (encodeUtf8Char dst ix '\xFFFD')
              else go =<< P.effect (encodeUtf8Char dst ix (w16ToChar w))
          _ -> P.fail InvalidEscapeSequence
        '"'# -> do
          str <- P.effect
            (PM.unsafeFreezeByteArray =<< PM.resizeMutableByteArray dst ix)
          pure (String (TS.fromShortByteStringUnsafe (byteArrayToShortByteString str)))
        _ -> go =<< P.effect (encodeUtf8Char dst ix (C# c))
  go 0

encodeUtf8Char :: MutableByteArray s -> Int -> Char -> ST s Int
encodeUtf8Char !marr !ix !c
  | c < '\128' = do
      PM.writeByteArray marr ix (c2w c)
      pure (ix + 1)
  | c < '\x0800' = do
      PM.writeByteArray marr ix
        (fromIntegral @Int @Word8 (unsafeShiftR (ord c) 6 .|. 0b11000000))
      PM.writeByteArray marr (ix + 1)
        (0b10000000 .|. (0b00111111 .&. (fromIntegral @Int @Word8 (ord c))))
      pure (ix + 2)
  | c <= '\xffff' = do
      PM.writeByteArray marr ix
        (fromIntegral @Int @Word8 (unsafeShiftR (ord c) 12 .|. 0b11100000))
      PM.writeByteArray marr (ix + 1)
        (0b10000000 .|. (0b00111111 .&. (fromIntegral @Int @Word8 (unsafeShiftR (ord c) 6))))
      PM.writeByteArray marr (ix + 2)
        (0b10000000 .|. (0b00111111 .&. (fromIntegral @Int @Word8 (ord c))))
      pure (ix + 3)
  | otherwise = error "encodeUtf8Char: write this"


-- Compute the maximum number of bytes that could possibly
-- be required to house the UTF-8-encoded string once any
-- JSON escape sequences have been resolved.
-- The correctness of this hinges on the assumption that
-- the UTF-8 encoding of a character never takes up more
-- bytes than its escape sequence.
-- TODO: Something fishy is going on with escape sequences
-- in this function. Look over this again.
string :: Int -> Parser TokenException s Token
string !start = go 1 where
  go !canMemcpy = do
    P.any IncompleteString >>= \case
      92 -> P.any InvalidEscapeSequence *> go 0 -- backslash
      34 -> do -- double quote
        !pos <- Unsafe.cursor
        case canMemcpy of
          1 -> do
            src <- Unsafe.expose
            str <- P.effect $ do
              let end = pos - 1
              let len = end - start
              dst <- PM.newByteArray len
              PM.copyByteArray dst 0 src start len
              PM.unsafeFreezeByteArray dst
            pure (String (TS.fromShortByteStringUnsafe (byteArrayToShortByteString str)))
          _ -> do
            Unsafe.unconsume (pos - start)
            let end = pos - 1
            let maxLen = end - start
            copyAndEscape maxLen
      W8# w -> go (canMemcpy .&. I# (ltWord# w 128##) .&. I# (gtWord# w 31##))

byteArrayToShortByteString :: ByteArray -> BSS.ShortByteString
byteArrayToShortByteString (PM.ByteArray x) = BSS.SBS x

c2w :: Char -> Word8
c2w = fromIntegral . ord

-- Precondition: Not in the range [U+D800 .. U+DFFF]
w16ToChar :: Word16 -> Char
w16ToChar (W16# w) = C# (chr# (word2Int# w))

