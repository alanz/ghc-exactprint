----------------------------------------------------------------------------
-- |
-- Module      :  FastTags.LexerM
-- Copyright   :  (c) Sergey Vinokurov 2019
--
-- All the types and functions needed to make lexer run:
-- - 'AlexInput' - primary workhorse, an optimized representation of input
--   stream as a pointer to utf8 bytes and our position within it.
-- - Lexer monad 'AlexM' - a monad (self-explanatory) with state that describes
--   current lexing context.
-- - 'AlexState' - state of the lexing monad, maintains current Alex code,
--   comment depth, quasiquoter depth, indentation size, whether we're in
--   a literate mode (and in which one) or vanilla mode and whether there
--   are any TH quasiquotes present till the end of file.
--
-- All the functions are to do with
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UnboxedTuples              #-}

module FastTags.LexerM
    ( AlexState(..)
    , mkAlexState
    , alexEnterBirdLiterateEnv
    , alexEnterLiterateLatexEnv
    , alexExitLiterateEnv
    , pushContext
    , modifyCommentDepth
    , modifyQuasiquoterDepth
    , modifyPreprocessorDepth
    , addIndentationSize
    , checkQuasiQuoteEndPresent

    , AlexM
    , runAlexM
    , alexSetInput
    , alexSetNextCode

    , AlexInput(..)
    , aiLineL
    , takeText
    , countInputSpace
    , extractDefineOrLetName
    , dropUntilNL
    , dropUntilUnescapedNL
    , dropUntilNLOr
    , dropUntilNLOrEither
    , unsafeTextHeadAscii
    , unsafeTextHeadOfTailAscii
    , unsafeTextHead
    , utf8BS

    , asCodeL
    , asCommentDepthL
    , asQuasiquoterDepthL
    , asIndentationSizeL
    , asPreprocessorDepthL
    , asLiterateLocL
    , asHaveQQEndL

      -- * Alex interface
    , alexInputPrevChar
    , alexGetByte
    ) where

import Control.Applicative as A
import Control.DeepSeq
import Control.Exception
import Control.Monad.ST
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict

import Data.Char
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Void (Void, vacuous)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Foreign.ForeignPtr
import Foreign.Ptr
import GHC.Base
import GHC.Ptr
import GHC.Word
import Text.Printf

import FastTags.LensBlaze
import FastTags.LexerTypes
import FastTags.Token

import GHC.Exts (word8ToWord#, wordToWord8#)


data AlexState = AlexState
    { asInput        :: {-# UNPACK #-} !AlexInput
    , asIntStore     :: {-# UNPACK #-} !Word64
        -- ^ Integer field that stores all the other useful fields for lexing.
    , asContextStack :: [Context]
    } deriving (Show, Eq, Ord)


checkQuasiQuoteEndPresent :: Ptr Word8 -> Bool
checkQuasiQuoteEndPresent
    = (\x -> isTrue# (qqessPresent x))
    . utf8Foldl' combine (QQEndsState 0# '\n'#)
    where
    combine :: QQEndsState -> Char# -> QQEndsState
    combine QQEndsState{qqessPresent, qqessPrevChar} c# = QQEndsState
        { qqessPresent      =
          qqessPresent `orI#`
          case (# qqessPrevChar, c# #) of
              (# '|'#, ']'# #) -> 1#
              (# _,    '⟧'# #) -> 1#
              _                -> 0#
        , qqessPrevChar = c#
        }

countInputSpace :: AlexInput -> Int -> Int
countInputSpace AlexInput{aiPtr} len =
    utf8FoldlBounded len inc 0 aiPtr
    where
    inc !acc ' '#  = acc + 1
    inc !acc '\t'# = acc + 8
    inc !acc c#    = case fixChar c# of
        1## -> acc + 1
        _   -> acc

-- Translate unicode character into special symbol we teached Alex to recognize.
{-# INLINE fixChar #-}
fixChar :: Char# -> Word#
fixChar = \case
    -- These should not be translated since Alex knows about them
    '→'#    -> reservedSym
    '∷'#    -> reservedSym
    '⇒'#    -> reservedSym
    '∀'#    -> reservedSym
    '⦇'#    -> reservedSym
    '⦈'#    -> reservedSym
    '⟦'#    -> reservedSym
    '⟧'#    -> reservedSym
    '\x00'# -> fullStop
    '\x01'# -> fullStop
    '\x02'# -> fullStop
    '\x03'# -> fullStop
    '\x04'# -> fullStop
    '\x05'# -> fullStop
    '\x06'# -> fullStop
    '\x07'# -> fullStop
    '\x08'# -> other
    c# -> case ord# c# of
        c2# | isTrue# (c2# <=# 0x7f#) ->
              int2Word# c2# -- Plain ascii needs no fixing.
            | otherwise   ->
                case generalCategory (C# c#) of
                    UppercaseLetter      -> upper
                    LowercaseLetter      -> lower
                    TitlecaseLetter      -> upper
                    ModifierLetter       -> suffix
                    OtherLetter          -> lower
                    NonSpacingMark       -> suffix
                    DecimalNumber        -> digit
                    OtherNumber          -> digit
                    Space                -> space
                    ConnectorPunctuation -> symbol
                    DashPunctuation      -> symbol
                    OtherPunctuation     -> symbol
                    MathSymbol           -> symbol
                    CurrencySymbol       -> symbol
                    ModifierSymbol       -> symbol
                    OtherSymbol          -> symbol

                    SpacingCombiningMark -> space
                    EnclosingMark        -> other
                    LetterNumber         -> symbol
                    OpenPunctuation      -> symbol
                    ClosePunctuation     -> symbol
                    InitialQuote         -> symbol
                    FinalQuote           -> symbol
                    LineSeparator        -> space
                    ParagraphSeparator   -> space
                    Control              -> other
                    Format               -> other
                    Surrogate            -> other
                    PrivateUse           -> other
                    NotAssigned          -> other
    where
      fullStop, space, upper, lower, symbol :: Word#
      digit, suffix, reservedSym, other :: Word#
      fullStop    = 0x00## -- Don't care about these
      space       = 0x01##
      upper       = 0x02##
      lower       = 0x03##
      symbol      = 0x04##
      digit       = 0x05##
      suffix      = 0x06##
      reservedSym = 0x07##
      other       = 0x08##

