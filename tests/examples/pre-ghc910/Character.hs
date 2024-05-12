{-# LANGUAGE QuasiQuotes #-}
module Atomo.Kernel.Character where

import Data.Char

import Atomo


load :: VM ()
load = do
    [p|(c: Character) control?|] =: liftM Boolean (onCharacter isControl)
    [p|(c: Character) space?|] =: liftM Boolean (onCharacter isSpace)
    [p|(c: Character) lower?|] =: liftM Boolean (onCharacter isLower)
    [p|(c: Character) upper?|] =: liftM Boolean (onCharacter isUpper)
    [p|(c: Character) alpha?|] =: liftM Boolean (onCharacter isAlpha)
    [p|(c: Character) alphanum?|] =: liftM Boolean (onCharacter isAlphaNum)
    [p|(c: Character) print?|] =: liftM Boolean (onCharacter isPrint)
    [p|(c: Character) digit?|] =: liftM Boolean (onCharacter isDigit)
    [p|(c: Character) oct-digit?|] =: liftM Boolean (onCharacter isOctDigit)
    [p|(c: Character) hex-digit?|] =: liftM Boolean (onCharacter isHexDigit)
    [p|(c: Character) letter?|] =: liftM Boolean (onCharacter isLetter)
    [p|(c: Character) mark?|] =: liftM Boolean (onCharacter isMark)
    [p|(c: Character) number?|] =: liftM Boolean (onCharacter isNumber)
    [p|(c: Character) punctuation?|] =: liftM Boolean (onCharacter isPunctuation)
    [p|(c: Character) symbol?|] =: liftM Boolean (onCharacter isSymbol)
    [p|(c: Character) separator?|] =: liftM Boolean (onCharacter isSeparator)
    [p|(c: Character) ascii?|] =: liftM Boolean (onCharacter isAscii)
    [p|(c: Character) latin1?|] =: liftM Boolean (onCharacter isLatin1)
    [p|(c: Character) ascii-upper?|] =: liftM Boolean (onCharacter isAsciiLower)
    [p|(c: Character) ascii-lower?|] =: liftM Boolean (onCharacter isAsciiUpper)

    [p|(c: Character) uppercase|] =: liftM Character (onCharacter toUpper)
    [p|(c: Character) lowercase|] =: liftM Character (onCharacter toLower)
    [p|(c: Character) titlecase|] =: liftM Character (onCharacter toTitle)

    [p|(c: Character) from-digit|] =: liftM (Integer . fromIntegral) (onCharacter digitToInt)
    [p|(i: Integer) to-digit|] =: liftM Character (onInteger (intToDigit . fromIntegral))

    [p|(c: Character) ord|] =: liftM (Integer . fromIntegral) (onCharacter ord)
    [p|(i: Integer) chr|] =: liftM Character (onInteger (chr . fromIntegral))

    [p|(c: Character) category|] =: liftM c (onCharacter generalCategory)
  where
    onCharacter :: (Char -> a) -> VM a
    onCharacter f = here "c" >>= liftM (f . fromCharacter) . findCharacter

    onInteger :: (Integer -> a) -> VM a
    onInteger f = here "i" >>= liftM (f . Atomo.fromInteger) . findInteger

    c UppercaseLetter = keyParticleN ["letter"] [particle "uppercase"]
    c LowercaseLetter = keyParticleN ["letter"] [particle "lowercase"]
    c TitlecaseLetter = keyParticleN ["letter"] [particle "titlecase"]
    c ModifierLetter = keyParticleN ["letter"] [particle "modified"]
    c OtherLetter = keyParticleN ["letter"] [particle "other"]
    c NonSpacingMark = keyParticleN ["mark"] [particle "non-spacing"]
    c SpacingCombiningMark = keyParticleN ["mark"] [particle "space-combining"]
    c EnclosingMark = keyParticleN ["mark"] [particle "enclosing"]
    c DecimalNumber = keyParticleN ["number"] [particle "decimal"]
    c LetterNumber = keyParticleN ["number"] [particle "letter"]
    c OtherNumber = keyParticleN ["number"] [particle "other"]
    c ConnectorPunctuation = keyParticleN ["punctuation"] [particle "connector"]
    c DashPunctuation = keyParticleN ["punctuation"] [particle "dash"]
    c OpenPunctuation = keyParticleN ["punctuation"] [particle "open"]
    c ClosePunctuation = keyParticleN ["punctuation"] [particle "close"]
    c InitialQuote = keyParticleN ["quote"] [particle "initial"]
    c FinalQuote = keyParticleN ["quote"] [particle "final"]
    c OtherPunctuation = keyParticleN ["punctuation"] [particle "other"]
    c MathSymbol = keyParticleN ["symbol"] [particle "math"]
    c CurrencySymbol = keyParticleN ["symbol"] [particle "currency"]
    c ModifierSymbol = keyParticleN ["symbol"] [particle "modifier"]
    c OtherSymbol = keyParticleN ["symbol"] [particle "other"]
    c Space = particle "space"
    c LineSeparator = keyParticleN ["separator"] [particle "line"]
    c ParagraphSeparator = keyParticleN ["separator"] [particle "paragraph"]
    c Control = particle "control"
    c Format = particle "format"
    c Surrogate = particle "surrogate"
    c PrivateUse = particle "private-use"
    c NotAssigned = particle "not-assigned"
