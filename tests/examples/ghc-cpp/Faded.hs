-- |
-- Module      :  Composition.Sound.Faded
-- Copyright   :  (c) OleksandrZhabenko 2020-2021
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music.
-- Uses SoX fade (in a special 2D way) effect and frequency modulation.
-- It uses 'Float' instead of 'Double'. This change is inspired by:
-- https://www.youtube.com/watch?v=FYTZkE5BZ-0
-- For conversion it uses functions from the 'GHC.Float' module.
-- Is rewritten from the dobutokO4 package.
--

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Composition.Sound.Faded (
  -- * Provide special faded effects and frequency modulation
  overChangeVolGNC
  , overChangeVolGN
  , overChangeVolG
  , overChangeVolGC
  , overChangeVolGF
  , overChangeVol
  , overChangeVolC
  , overChangeVolF
  , overChangeVolGCN
  , overChangeVolGFN
  , overChangeVolN
  , overChangeVolCN
  , overChangeVolFN
  -- * Mixing function
  , mixGTest
  , mixGTestN
  -- * Generate several files
  , basicFN
  , basicF
  , basicFC
  , basicF2
  , basicF2C
  , basicFCN
  , basicF2N
  , basicF2CN
  -- * Generate several files with frequency modulation
  , moreFNC
  , moreFN
  , moreFCN
  , reverbFix
  -- * Auxiliary functions
  , endingWF
  , charFadeType
  , argString
  , freqChange
  -- * Special numbers
  , sameConst
) where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif
import System.Exit (ExitCode (ExitSuccess))
import Data.List (isPrefixOf,isSuffixOf)
import Data.Maybe (fromJust)
import System.Process
import EndOfExe (showE)
import MMSyn7l (fadeEndsTMB,fadeEndsTMN)
import Numeric (showFFloat)
import System.Directory
import Composition.Sound.Functional.Basics
import Composition.Sound.IntermediateF (soxBasicParams)
import Data.DoubleZip (evalSndFV)

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

-- | Generates a sound, the volume of which (being plotted graphically) comes through the given 2D points at the time-volume scale with possibly
-- changing frequency (they are specified by the first and the second 'Float' arguments). Uses SoX inside especially the \"fade\" and \"synth\" effects.
-- For the equal frequencies generates specifically faded output without frequency modulation.
overChangeVolG :: String -> String -> Int -> Float -> Float -> Float -> Float -> ((Float,Float), (Float,Float)) -> IO ()
overChangeVolG = overChangeVolGN "test"
{-# INLINE overChangeVolG #-}

-- | A generalized version of the 'overChangeVolG' with a possibility to specify the name of the resulting file (by default it is \"test\" based).
overChangeVolGN :: FilePath -> String -> String -> Int -> Float -> Float -> Float -> Float -> ((Float,Float), (Float,Float)) -> IO ()
overChangeVolGN filestart ys cs j freq1 freq2 x0 xdelta ((t0,v0), (t1,v1))
 | x0 /= 0 && abs x0 <= 1.0 && freq1 > 16 && freq1 < 20000 =
  case compare (v1 * v0) 0 of
    GT -> do
     (code1,_,herr1) <- readProcessWithExitCode (fromJust (showE "sox")) (soxBasicParams ys ["","-n","test1.wav","synth",
       showFFloat Nothing (if t1 == t0 then abs x0 else abs (t1 - t0)) "", "sine", showFFloat (Just 4) freq1 (freqChange (drop 1 cs) freq2 freq1), "fade",
        charFadeType (if null cs then 'l' else head cs)] ++
         if compare ((v1 - v0) * (t1 - t0)) 0 /= LT then [showFFloat Nothing (if compare (abs (t1 - t0)) xdelta /= GT then abs x0 else abs (t1 - t0)) ""]
           else ["0", "-0.0", showFFloat Nothing (if compare (abs (t1 - t0)) xdelta /= GT then abs x0 else abs (t1 - t0)) "", "vol",
             showFFloat (Just 4) (signum v1 * abs (v1 - v0)) ""]) ""
     if code1 /= ExitSuccess then error $ "Composition.Sound.Faded.overChangeVolGN: " ++ herr1
     else do
      (code2,_,herr2) <- readProcessWithExitCode (fromJust (showE "sox")) (soxBasicParams ys ["","-n","test0.wav","synth",
        showFFloat Nothing (if compare (abs (t1 - t0)) xdelta /= GT then abs x0 else abs (t1 - t0)) "", "sine",
          showFFloat (Just 4) freq1 (freqChange (drop 1 cs) freq2 freq1), "vol", showFFloat (Just 4) (min v0 v1) ""]) ""
      if code2 == ExitSuccess
        then do
          (code3,_,herr3) <- readProcessWithExitCode (fromJust (showE "sox")) ["-m","test0" ++ endingWF ys,"test1" ++ endingWF ys, (filestart ++ "G") ++
            prependZeroes 6 (show j) ++ endingWF ys, "vol", "2"] ""
          if code3 == ExitSuccess
            then removeFile ("test0" ++ endingWF ys) >> removeFile ("test1" ++ endingWF ys)
            else error $ "Composition.Sound.Faded.overChangeVolGN: " ++ herr3
        else print herr2 >> error "Composition.Sound.Faded.overChangeVolGN: Operation not successful. "
    LT -> do
     overChangeVolGN filestart ys cs j freq1 ((v0 * freq2 - v1 * freq1) / (v0 - v1)) x0 xdelta ((t0,v0), ((v0 * t1 - v1 * t0) / (v0 - v1),0)) >>
      renameFile ((filestart ++ "G") ++ prependZeroes 6 (show j) ++ endingWF ys) ("temp00" ++ endingWF ys)
     (code0,_,herr) <- readProcessWithExitCode (fromJust (showE "sox")) ["temp00" ++ endingWF ys,"temp0" ++ endingWF ys, "fade", "h", "0", "-0.0",
       showFFloat (Just 6) ((max freq1 freq2) ** (-1.0)) ""] ""
     if code0 /= ExitSuccess then error (show herr)
     else do
      overChangeVolGN filestart ys cs j ((v0 * freq2 - v1 * freq1) / (v0 - v1)) freq2 x0 xdelta (((v0 * t1 - v1 * t0) / (v0 - v1),0), (t1,v1)) >>
       renameFile ((filestart ++ "G") ++ prependZeroes 6 (show j) ++ endingWF ys) ("temp1" ++ endingWF ys)
      (code1,_,herr1) <- readProcessWithExitCode (fromJust (showE "sox")) ["temp0" ++ endingWF ys,"temp1" ++ endingWF ys, (filestart ++ "G") ++
       prependZeroes 6 (show j) ++ endingWF ys] ""
      if code1 == ExitSuccess then removeFile ("temp0" ++ endingWF ys) >> removeFile ("temp1" ++ endingWF ys) >> removeFile ("temp00" ++ endingWF ys)
      else error $ "Composition.Sound.Faded.overChangeVolGN: " ++ herr1
    _  ->
     case v1 of
      0 ->
       if v0 == 0
        then do
          (code1,_,herr1) <- readProcessWithExitCode (fromJust (showE "sox")) (soxBasicParams ys ["","-n",(filestart ++ "G") ++ prependZeroes 6 (show j) ++
            ".wav","delay", showFFloat Nothing (if compare (abs (t1 - t0)) xdelta /= GT then abs x0 else abs (t1 - t0)) "", "trim", showFFloat Nothing
              (if compare (abs (t1 - t0)) xdelta /= GT then abs x0 else abs (t1 - t0)) ""]) ""
          if code1 /= ExitSuccess then error $ "Composition.Sound.Faded.overChangeVolGN: " ++ herr1
          else return ()
        else do
          (code1,_,herr1) <- readProcessWithExitCode (fromJust (showE "sox")) (soxBasicParams ys ["","-n",(filestart ++ "G") ++ prependZeroes 6 (show j) ++
            ".wav","synth", showFFloat Nothing (if compare (abs (t1 - t0)) xdelta /= GT then abs x0 else abs (t1 - t0)) "", "sine",
              showFFloat (Just 4) freq1 (freqChange (drop 1 cs) freq2 freq1), "fade", charFadeType (if null cs then 'l' else head cs)] ++
               if compare t0 t1 == GT then [showFFloat Nothing
                (if compare (abs (t1 - t0)) xdelta /= GT then abs x0 else abs (t1 - t0)) ""] else ["0", "-0.0", showFFloat Nothing
                  (if compare (abs (t1 - t0)) xdelta /= GT then abs x0 else abs (t1 - t0)) "", "vol", showFFloat (Just 4) v0 ""]) ""
          if code1 /= ExitSuccess then error $ "Composition.Sound.Faded.overChangeVolGN: " ++ herr1
          else return ()
      _ -> do
       (code1,_,herr1) <- readProcessWithExitCode (fromJust (showE "sox")) (soxBasicParams ys ["","-n",(filestart ++ "G") ++ prependZeroes 6 (show j) ++ ".wav",
         "synth", showFFloat Nothing (if compare (abs (t1 - t0)) xdelta /= GT then abs x0 else abs (t1 - t0)) "", "sine",
          showFFloat (Just 4) freq1 (freqChange (drop 1 cs) freq2 freq1), "fade", charFadeType (if null cs then 'l' else head cs)] ++
           if compare t1 t0 == GT
            then [showFFloat Nothing (if compare (abs (t1 - t0)) xdelta /= GT then abs x0
             else abs (t1 - t0)) ""] else ["0", "-0.0", showFFloat Nothing (if compare (abs (t1 - t0)) xdelta /= GT then abs x0 else abs (t1 - t0)) "",
               "vol", showFFloat (Just 4) v1 ""]) ""
       if code1 /= ExitSuccess then error $ "Composition.Sound.Faded.overChangeVolGN: " ++ herr1
       else return ()
 | otherwise = error "Composition.Sound.Faded.overChangeVolGN: sound for these conditions is not defined. "

freqChange :: String -> Float -> Float -> String
freqChange xs freq freq1
 | freq >= 16 && freq <= 20000 = if freq /= freq1 then
   case xs of
    "l" -> ':':showFFloat (Just 4) freq ""
    "s" -> '+':showFFloat (Just 4) freq ""
    "e" -> '/':showFFloat (Just 4) freq ""
    _ -> '-':showFFloat (Just 4) freq ""
     else ""
 | otherwise = error "Composition.Sound.Faded.freqChange: undefined for this value of the frequency (the first Float argument). "

-- | Generates a sound, the volume of which (being plotted graphically) comes through the given 2D points at the time-volume scale. Uses SoX inside
-- especially the \"fade\" and \"synth\" effects. A frequency does not change and is specified (in Hz) by the first 'Float' argument.
overChangeVol :: String -> Char -> Int -> Float -> Float -> Float -> ((Float,Float), (Float,Float)) -> IO ()
overChangeVol ys c j freq1 = overChangeVolGN "test" ys [c] j freq1 freq1
{-# INLINE overChangeVol #-}

-- | A generalized version of the 'overChangeVol' with a possibility to specify the name for the mixed files (by default is \"test\" based).
overChangeVolN :: FilePath -> String -> Char -> Int -> Float -> Float -> Float -> ((Float,Float), (Float,Float)) -> IO ()
overChangeVolN filestart ys c j freq1 = overChangeVolGN filestart ys [c] j freq1 freq1
{-# INLINE overChangeVolN #-}

-- | Generates a sound, the volume of which (being plotted graphically) comes through the given 2D points in the time-volume scale. Uses SoX inside especially
-- the \"fade\" and \"synth\" effects. A frequency does not change and is specified (in Hz) by the first 'Float' argument. Is a curried variant of the
-- 'overChangeVol' in its two last arguments.
overChangeVolC :: String -> Char -> Int -> Float -> Float -> Float -> (Float,Float) -> (Float,Float) -> IO ()
overChangeVolC ys c j freq x0 xdelta w1 = overChangeVol ys c j freq x0 xdelta . (,) w1
{-# INLINE overChangeVolC #-}

-- | A generalized version of the 'overChangeVolC' with a possibility to specify the name for the mixed files (by default is \"test\" based).
overChangeVolCN :: FilePath -> String -> Char -> Int -> Float -> Float -> Float -> (Float,Float) -> (Float,Float) -> IO ()
overChangeVolCN filestart ys c j freq x0 xdelta w1 = overChangeVolN filestart ys c j freq x0 xdelta . (,) w1
{-# INLINE overChangeVolCN #-}

-- | Generates a sound, the volume of which (being plotted graphically) comes through the given 2D points in the time-volume scale with possibly changing frequency (they are specified by
-- the first and the second 'Float' arguments). Uses SoX inside especially the \"fade\" and \"synth\" effects. For the equal frequencies generates specifically
-- faded output without frequency modulation. Is a curried variant of the 'overChangeVolG' in its two last arguments.
overChangeVolGC :: String -> String -> Int -> Float -> Float -> Float -> Float -> (Float,Float) -> (Float,Float) -> IO ()
overChangeVolGC ys cs j freq1 freq2 x0 xdelta w1 = overChangeVolG ys cs j freq1 freq2 x0 xdelta . (,) w1
{-# INLINE overChangeVolGC #-}

-- | A generalized version of the 'overChangeVolGC' with a possibility to specify the name for the mixed files (by default is \"test\" based).
overChangeVolGCN :: FilePath -> String -> String -> Int -> Float -> Float -> Float -> Float -> (Float,Float) -> (Float,Float) -> IO ()
overChangeVolGCN filestart ys cs j freq1 freq2 x0 xdelta w1 = overChangeVolGN filestart ys cs j freq1 freq2 x0 xdelta . (,) w1
{-# INLINE overChangeVolGCN #-}

-- | Generates a sound, the volume of which (being plotted graphically) comes through the given 2D points in the time-volume scale. Uses SoX inside especially
-- the \"fade\" and \"synth\" effects. Is a somewhat flipped variant of the 'overChangeVol' with changed order of the arguments (is provided here
-- for convenience).
overChangeVolF :: String -> Char -> Int -> Float -> Float -> (Float,Float) -> (Float,Float) ->  Float -> IO ()
overChangeVolF ys c j x0 xdelta w1 w2 freq  = overChangeVol ys c j freq x0 xdelta (w1,w2)
{-# INLINE overChangeVolF #-}

-- | A generalized version of the 'overChangeVolF' with a possibility to specify the name for the mixed files (by default is \"test\" based).
overChangeVolFN :: FilePath -> String -> Char -> Int -> Float -> Float -> (Float,Float) -> (Float,Float) ->  Float -> IO ()
overChangeVolFN filestart ys c j x0 xdelta w1 w2 freq  = overChangeVolN filestart ys c j freq x0 xdelta (w1,w2)
{-# INLINE overChangeVolFN #-}

-- | Generates a sound, the volume of which (being plotted graphically) comes through the given 2D points in the time-volume scale with possibly
-- changing frequency (they are specified by the first and the second 'Float' arguments). Uses SoX inside especially the \"fade\" and \"synth\" effects.
-- For the equal frequencies generates specifically faded output without frequency modulation. Is a somewhat flipped variant of the 'overChangeVolGC'
-- with changed order of the arguments (is provided here for convenience).
overChangeVolGF :: String -> String -> Int -> Float -> Float -> (Float,Float) -> (Float,Float) ->  Float -> Float -> IO ()
overChangeVolGF ys cs j x0 xdelta w1 w2 freq1 freq2  = overChangeVolG ys cs j freq1 freq2 x0 xdelta (w1,w2)
{-# INLINE overChangeVolGF #-}

-- | A generalized version of the 'overChangeVolGF' with a possibility to specify the name for the mixed files (by default is \"test\" based).
overChangeVolGFN :: FilePath -> String -> String -> Int -> Float -> Float -> (Float,Float) -> (Float,Float) ->  Float -> Float -> IO ()
overChangeVolGFN filestart ys cs j x0 xdelta w1 w2 freq1 freq2  = overChangeVolGN filestart ys cs j freq1 freq2 x0 xdelta (w1,w2)
{-# INLINE overChangeVolGFN #-}

-- | A simplified variant of the 'soxBasicParameters' function with defining only a file extension.
endingWF :: String -> String
endingWF ys
 | not (null ys) = if last ys == 'f' then ".flac" else ".wav"
 | otherwise = ".wav"

-- | Converts a character into a corresponding string using \"l\" (a logarithmic one) as the default one. An output can specify then the fade type for SoX.
charFadeType :: Char -> String
charFadeType c =
  case c of
   'h' -> "h"
   'p' -> "p"
   't' -> "t"
   _  -> "l"

-- | Using SoX mixes all the \"testG*\" (of the WAV or FLAC extension specified by the 'String' argument -- see 'endingWF') in the current directory.
-- If there are \"resultG.*" (wav or flac respectively) file in the directory, it is overwritten. Also the "testG*" files are deleted afterwards if the
-- mixing is successful.
mixGTest :: String -> IO ()
mixGTest ys = do
  dir <- listDirectory "."
  (code1,_,herr1) <- readProcessWithExitCode (fromJust (showE "sox")) (mconcat [["-m"], filter (\xs -> "testG" `isPrefixOf` xs &&
    endingWF ys `isSuffixOf` xs) dir, ["resultG" ++ endingWF ys]]) ""
  if code1 /= ExitSuccess then error $ "Composition.Sound.Faded.mixGTest: " ++ herr1
  else mapM_ removeFile . filter (\xs -> "testG" `isPrefixOf` xs && endingWF ys `isSuffixOf` xs) $ dir

-- | A generalized version of the 'mixGTest' with a possibility to specify the name for the mixed files (by default is \"test\" based).
mixGTestN :: FilePath -> String -> IO ()
mixGTestN filestart ys = do
  dir <- listDirectory "."
  (code1,_,herr1) <- readProcessWithExitCode (fromJust (showE "sox")) (mconcat [["-m"], filter (\xs -> (filestart ++ "G") `isPrefixOf` xs &&
    endingWF ys `isSuffixOf` xs) dir, ["resultG" ++ endingWF ys]]) ""
  if code1 /= ExitSuccess then error $ "Composition.Sound.Faded.mixGTestN: " ++ herr1
  else mapM_ removeFile . filter (\xs -> (filestart ++ "G") `isPrefixOf` xs && endingWF ys `isSuffixOf` xs) $ dir

-- | Generates a sequence of sounds using 'overChangeVol' so that their time-volume characteristic is going through the 2D points obtained
-- with the last two arguments.
-- Uses 'fadeEndsTMB', the arguments for which are specified by the second symbol in the second 'String' and by the third 'Float' argument.
basicF :: String -> String -> Float -> Float -> Float -> Float -> (Float -> Float) -> [Float] -> IO ()
basicF ys x2s freq x0 xdelta per f v = do
  let (xs1,xs2) = splitAt 1 x2s
      c1
       | null xs1 = 'l'
       | otherwise = head xs1
      c2
       | null xs2 = 'l'
       | otherwise = head xs2
  v1 <- evalSndFV f v
  mapM_ (\(i, x) -> do
    overChangeVol ys c1 i freq x0 xdelta x
    fadeEndsTMB c2 per $ "testG" ++ prependZeroes 6 (show i) ++ endingWF ys) . zip [0..] $ v1

-- | A generalized version of the 'basicF' with a possibility to specify the name for the generated files (by default is \"test\" based).
basicFN :: FilePath -> String -> String -> Float -> Float -> Float -> Float -> (Float -> Float) -> [Float] -> IO ()
basicFN filestart ys x2s freq x0 xdelta per f v = do
  let (xs1,xs2) = splitAt 1 x2s
      c1
       | null xs1 = 'l'
       | otherwise = head xs1
      c2
       | null xs2 = 'l'
       | otherwise = head xs2
  v1 <- evalSndFV f v
  mapM_ (\(i, x) -> do
    overChangeVolGN filestart ys [c1] i freq freq x0 xdelta x
    fadeEndsTMB c2 per $ (filestart ++ "G") ++ prependZeroes 6 (show i) ++ endingWF ys) . zip [0..] $ v1

-- | Splits its argument (the first six symbols if present) (like 'splitAt') into two 'String' with the length of (if possible) 4 and 2 characters.
-- The rest of the argument is not used.
argString :: String -> (String,String)
argString xs = (take 4 xs,take 2 . drop 4 $ xs)
{-# INLINE argString #-}

-- | Generates a sequence of sounds using 'overChangeVol' so that their time-volume characteristic (if being plotted graphically) is going through
-- the 2D points obtained with the last two arguments.
-- The 'String' should consist of 6 alphanumeric characters. The first four as for the 'soxBasicParams', and the fifth one -- a letter from the \"hlpqt\". The
-- sixth one is one of the \"els\" or some other symbol.
-- Otherwise, the default values are used (\"221w\" for the first and \"ll\" for the second one).
basicFC :: String -> Float -> Float -> Float -> Float -> (Float -> Float) -> [Float] -> IO ()
basicFC = basicFCN "test"
{-# INLINE basicFC #-}

-- | A generalized version of the 'basicFC' with a possibility to specify the name for the mixed files (by default is \"test\" based).
basicFCN :: FilePath -> String -> Float -> Float -> Float -> Float -> (Float -> Float) -> [Float] -> IO ()
basicFCN filestart xs freq x0 xdelta per f v = let (ys,x2s) = argString xs in basicFN filestart ys x2s freq x0 xdelta per f v
{-# INLINE basicFCN #-}

-- | Generates a sequence of sounds using 'overChangeVol' so that their time-volume characteristic (if being plotted graphically) is going through the 2D points obtained
-- with the last two arguments.
-- Uses 'fadeEndsTMN', the arguments for which are specified by the second symbol in the second 'String' and by the third and fourth 'Float' arguments.
basicF2 :: String -> String -> Float -> Float -> Float -> Float -> Float -> (Float -> Float) -> [Float] -> IO ()
basicF2 = basicF2N "test"
{-# INLINE basicF2 #-}

-- | A generalized version of the 'basicF2' with a possibility to specify the name for the mixed files (by default is \"test\" based).
basicF2N :: FilePath -> String -> String -> Float -> Float -> Float -> Float -> Float -> (Float -> Float) -> [Float] -> IO ()
basicF2N filestart ys x2s freq x0 xdelta per1 per2 f v = do
  let (xs1,xs2) = splitAt 1 x2s
      c1
       | null xs1 = 'l'
       | otherwise = head xs1
      c2
       | null xs2 = 'l'
       | otherwise = head xs2
  v1 <- evalSndFV f v
  mapM_ (\(i, x) -> do
    overChangeVolN filestart ys c1 i freq x0 xdelta x
    fadeEndsTMN c2 per1 per2 $ (filestart ++ "G") ++ prependZeroes 6 (show i) ++ endingWF ys) . zip [0..] $ v1

-- | Generates a sequence of sounds using 'overChangeVol' so that their time-volume characteristic (if being plotted graphically) is going through
-- the 2D points obtained with the last two arguments.
-- The 'String' should consist of 6 alphanumeric characters. The first four as for the 'soxBasicParams' and the the fifth one -- a letter
-- from the \"hlpqt\". The sixth one is one of the \"els\" or some other symbol. Otherwise, the default values are used (\"221w\" for the first
-- and \"ll\" for the second one).
basicF2C :: String -> Float -> Float -> Float -> Float -> Float -> (Float -> Float) -> [Float] -> IO ()
basicF2C = basicF2CN "test"
{-# INLINE basicF2C #-}

-- | A generalized version of the 'basicF2C' with a possibility to specify the name for the mixed files (by default is \"test\" based).
basicF2CN :: FilePath -> String -> Float -> Float -> Float -> Float -> Float -> (Float -> Float) -> [Float] -> IO ()
basicF2CN filestart xs freq x0 xdelta per1 per2 f v = let (ys,x2s) = argString xs in basicF2N filestart ys x2s freq x0 xdelta per1 per2 f v
{-# INLINE basicF2CN #-}

-- | A generalized version of the 'basicFN' with a frequency modulation.
moreFN :: FilePath -> String -> String -> Float -> Float -> Float -> Float -> Float -> (Float -> Float) -> [Float] -> IO ()
moreFN filestart ys x2s freq1 freq2 x0 xdelta per f v = do
  let (xs1,xs2) = splitAt 1 x2s
      c1
       | null xs1 = 'l'
       | otherwise = head xs1
      c2
       | null xs2 = 'l'
       | otherwise = head xs2
  v1 <- evalSndFV f v
  mapM_ (\(i, x) -> do
    overChangeVolGN filestart ys [c1] i freq1 freq2 x0 xdelta x
    fadeEndsTMB c2 per $ (filestart ++ "G") ++ prependZeroes 6 (show i) ++ endingWF ys) . zip [0..] $ v1

-- | A generalized version of the 'basicFCN' with a frequency modulation.
moreFCN :: FilePath -> String -> Float -> Float -> Float -> Float -> Float -> (Float -> Float) -> [Float] -> IO ()
moreFCN filestart xs freq1 freq2 x0 xdelta per f v = let (ys,x2s) = argString xs in moreFN filestart ys x2s freq1 freq2 x0 xdelta per f v
{-# INLINE moreFCN #-}

--------------------------------------------------------------------------------------------

-- | Approximately equals to 2 ** (1/48) if the argument is zero (0) and the inverse value otherwise.
sameConst :: Int -> Float
sameConst i
 | i == 0 = 0.1 * (7.0 + pi)
 | otherwise = 10.0 / (7.0 + pi)
{-# INLINE sameConst #-}

-- | A generalized version of the 'overChangeVolGN' with a possibility to make lower the noisy clipping by specifying the first parameter. The default parameter
-- \"rev\" it uses reverberation to transform the distortion on the edges in case of existing root on fro the 2D line connection points.
overChangeVolGNC :: String -> FilePath -> String -> String -> Int -> Float -> Float -> Float -> Float -> ((Float,Float), (Float,Float)) -> IO ()
overChangeVolGNC check filestart ys cs j freq1 freq2 x0 xdelta ((t0,v0), (t1,v1))
 | x0 /= 0 && abs x0 <= 1.0 && freq1 > 16 && freq1 < 20000 =
  case compare (v1 * v0) 0 of
    GT -> do
     (code1,_,herr1) <- readProcessWithExitCode (fromJust (showE "sox")) (soxBasicParams ys ["","-n","test1.wav","synth",
       showFFloat Nothing (if t1 == t0 then abs x0 else abs (t1 - t0)) "", "sine", showFFloat (Just 4) freq1 (freqChange (drop 1 cs) freq2 freq1), "fade",
        charFadeType (if null cs then 'l' else head cs)] ++
         if compare ((v1 - v0) * (t1 - t0)) 0 /= LT then [showFFloat Nothing (if compare (abs (t1 - t0)) xdelta /= GT then abs x0 else abs (t1 - t0)) ""]
           else ["0", "-0.0", showFFloat Nothing (if compare (abs (t1 - t0)) xdelta /= GT then abs x0 else abs (t1 - t0)) "", "vol",
             showFFloat (Just 4) (signum v1 * abs (v1 - v0)) ""]) ""
     if code1 /= ExitSuccess then error $ "Composition.Sound.Faded.overChangeVolGNC: " ++ herr1
     else do
      (code2,_,herr2) <- readProcessWithExitCode (fromJust (showE "sox")) (soxBasicParams ys ["","-n","test0.wav","synth",
        showFFloat Nothing (if compare (abs (t1 - t0)) xdelta /= GT then abs x0 else abs (t1 - t0)) "", "sine",
          showFFloat (Just 4) freq1 (freqChange (drop 1 cs) freq2 freq1), "vol", showFFloat (Just 4) (min v0 v1) ""]) ""
      if code2 == ExitSuccess
        then do
          (code3,_,herr3) <- readProcessWithExitCode (fromJust (showE "sox")) ["-m","test0" ++ endingWF ys,"test1" ++ endingWF ys, (filestart ++ "G") ++
            prependZeroes 6 (show j) ++ endingWF ys, "vol", "2"] ""
          if code3 == ExitSuccess
            then removeFile ("test0" ++ endingWF ys) >> removeFile ("test1" ++ endingWF ys)
            else error $ "Composition.Sound.Faded.overChangeVolGNC: " ++ herr3
        else print herr2 >> error "Composition.Sound.Faded.overChangeVolGNC: Operation not successful. "
    LT ->
     case check of
      "simple" -> do
        overChangeVolGNC check filestart ys cs j freq1 ((v0 * freq2 - v1 * freq1) / (v0 - v1)) x0 xdelta ((t0,v0), ((v0 * t1 - v1 * t0) / (v0 - v1),0)) >>
         renameFile ((filestart ++ "G") ++ prependZeroes 6 (show j) ++ endingWF ys) ("temp00" ++ endingWF ys)
        (code0,_,herr) <- readProcessWithExitCode (fromJust (showE "sox")) ["temp00" ++ endingWF ys,"temp0" ++ endingWF ys, "fade", "h", "0", "-0.0",
          showFFloat (Just 6) ((max freq1 freq2) ** (-1.0)) ""] ""
        if code0 /= ExitSuccess then error (show herr)
        else do
         overChangeVolGNC check filestart ys cs j ((v0 * freq2 - v1 * freq1) / (v0 - v1)) freq2 x0 xdelta (((v0 * t1 - v1 * t0) / (v0 - v1),0), (t1,v1)) >>
           renameFile ((filestart ++ "G") ++ prependZeroes 6 (show j) ++ endingWF ys) ("temp1" ++ endingWF ys)
         (code1,_,herr1) <- readProcessWithExitCode (fromJust (showE "sox")) ["temp0" ++ endingWF ys,"temp1" ++ endingWF ys, (filestart ++ "G") ++
            prependZeroes 6 (show j) ++ endingWF ys] ""
         if code1 == ExitSuccess then removeFile ("temp0" ++ endingWF ys) >> removeFile ("temp1" ++ endingWF ys) >> removeFile ("temp00" ++ endingWF ys)
         else error $ "Composition.Sound.Faded.overChangeVolGNC: " ++ herr1
      "silent" -> do
        (code0,_,herr) <- readProcessWithExitCode (fromJust (showE "sox")) (soxBasicParams ys ["","-n","temp0" ++ endingWF ys,"trim", "0",
         showFFloat (Just 6) (abs (v0 * (t0 - t1) / (v0 - v1))) "","vol","0"]) ""
        if code0 /= ExitSuccess then error (show herr)
        else do
         overChangeVolGNC check filestart ys cs j ((v0 * freq2 - v1 * freq1) / (v0 - v1)) freq2 x0 xdelta (((v0 * t1 - v1 * t0) / (v0 - v1),0), (t1,v1)) >>
           renameFile ((filestart ++ "G") ++ prependZeroes 6 (show j) ++ endingWF ys) ("temp1" ++ endingWF ys)
         (code1,_,herr1) <- readProcessWithExitCode (fromJust (showE "sox")) ["temp0" ++ endingWF ys,"temp1" ++ endingWF ys, (filestart ++ "G") ++
           prependZeroes 6 (show j) ++ endingWF ys] ""
         if code1 == ExitSuccess then removeFile ("temp0" ++ endingWF ys) >> removeFile ("temp1" ++ endingWF ys)
         else error $ "Composition.Sound.Faded.overChangeVolGNC: " ++ herr1
      _ -> do
        (code0,_,herr) <- readProcessWithExitCode (fromJust (showE "sox")) (soxBasicParams ys ["","-n","temp0" ++ endingWF ys,"trim", "0",
         showFFloat (Just 6) (abs (v0 * (t0 - t1) / (v0 - v1))) "","vol","0"]) ""
        if code0 /= ExitSuccess then error (show herr)
        else do
         overChangeVolGNC check filestart ys cs j ((v0 * freq2 - v1 * freq1) / (v0 - v1)) freq2 x0 xdelta (((v0 * t1 - v1 * t0) / (v0 - v1),0), (t1,v1)) >>
           renameFile ((filestart ++ "G") ++ prependZeroes 6 (show j) ++ endingWF ys) ("temp1" ++ endingWF ys)
         (code1,_,herr1) <- readProcessWithExitCode (fromJust (showE "sox")) ["temp0" ++ endingWF ys,"temp1" ++ endingWF ys, (filestart ++ "G") ++
           prependZeroes 6 (show j) ++ endingWF ys,"reverb","-w","1","1","10","gain","-n","gain","-9"] ""
         if code1 == ExitSuccess then removeFile ("temp0" ++ endingWF ys) >> removeFile ("temp1" ++ endingWF ys)
         else error $ "Composition.Sound.Faded.overChangeVolGNC: " ++ herr1
    _  ->
     case v1 of
      0 ->
       if v0 == 0
        then do
          (code1,_,herr1) <- readProcessWithExitCode (fromJust (showE "sox")) (soxBasicParams ys ["","-n",(filestart ++ "G") ++ prependZeroes 6 (show j) ++
            ".wav","delay", showFFloat Nothing (if compare (abs (t1 - t0)) xdelta /= GT then abs x0 else abs (t1 - t0)) "", "trim", showFFloat Nothing
              (if compare (abs (t1 - t0)) xdelta /= GT then abs x0 else abs (t1 - t0)) ""]) ""
          if code1 /= ExitSuccess then error $ "Composition.Sound.Faded.overChangeVolGNC: " ++ herr1
          else return ()
        else do
          (code1,_,herr1) <- readProcessWithExitCode (fromJust (showE "sox")) (soxBasicParams ys ["","-n",(filestart ++ "G") ++ prependZeroes 6 (show j) ++
            ".wav","synth", showFFloat Nothing (if compare (abs (t1 - t0)) xdelta /= GT then abs x0 else abs (t1 - t0)) "", "sine",
              showFFloat (Just 4) freq1 (freqChange (drop 1 cs) freq2 freq1), "fade", charFadeType (if null cs then 'l' else head cs)] ++
               if compare t0 t1 == GT then [showFFloat Nothing
                (if compare (abs (t1 - t0)) xdelta /= GT then abs x0 else abs (t1 - t0)) ""] else ["0", "-0.0", showFFloat Nothing
                  (if compare (abs (t1 - t0)) xdelta /= GT then abs x0 else abs (t1 - t0)) "", "vol", showFFloat (Just 4) v0 ""]) ""
          if code1 /= ExitSuccess then error $ "Composition.Sound.Faded.overChangeVolGNC: " ++ herr1
          else return ()
      _ -> do
       (code1,_,herr1) <- readProcessWithExitCode (fromJust (showE "sox")) (soxBasicParams ys ["","-n",(filestart ++ "G") ++ prependZeroes 6 (show j) ++ ".wav",
         "synth", showFFloat Nothing (if compare (abs (t1 - t0)) xdelta /= GT then abs x0 else abs (t1 - t0)) "", "sine",
          showFFloat (Just 4) freq1 (freqChange (drop 1 cs) freq2 freq1), "fade", charFadeType (if null cs then 'l' else head cs)] ++
           if compare t1 t0 == GT
            then [showFFloat Nothing (if compare (abs (t1 - t0)) xdelta /= GT then abs x0
             else abs (t1 - t0)) ""] else ["0", "-0.0", showFFloat Nothing (if compare (abs (t1 - t0)) xdelta /= GT then abs x0 else abs (t1 - t0)) "",
               "vol", showFFloat (Just 4) v1 ""]) ""
       if code1 /= ExitSuccess then error $ "Composition.Sound.Faded.overChangeVolGNC: " ++ herr1
       else return ()
 | otherwise = error "Composition.Sound.Faded.overChangeVolGNC: sound for these conditions is not defined. "

-- | A generalized version of the 'moreFN' with a possibility to change the behaviour for the situation with existing roots on the interval for the line connecting the 2D
-- points.
moreFNC :: String -> FilePath -> String -> String -> Float -> Float -> Float -> Float -> Float -> (Float -> Float) -> [Float] -> IO ()
moreFNC check filestart ys x2s freq1 freq2 x0 xdelta per f v = do
  let (xs1,xs2) = splitAt 1 x2s
      c1
       | null xs1 = 'l'
       | otherwise = head xs1
      c2
       | null xs2 = 'l'
       | otherwise = head xs2
  v1 <- evalSndFV f v
  mapM_ (\(i, x) -> do
    overChangeVolGNC check filestart ys [c1] i freq1 freq2 x0 xdelta x
    fadeEndsTMB c2 per $ (filestart ++ "G") ++ prependZeroes 6 (show i) ++ endingWF ys
    reverbFix $ (filestart ++ "G") ++ prependZeroes 6 (show i) ++ endingWF ys) . zip [0..] $ v1

-- | Auxiliary function that can be used to apply a \"reverb\" effect or in this module context to fix unpleasant noise for concatenated parts. Usually, can be used after
-- application of the other functions in the module to transform the noise into more music sound. It is usually applied after all the other functions.
reverbFix :: FilePath -> IO ()
reverbFix file = do
  (code,_,herr) <- readProcessWithExitCode (fromJust (showE "sox")) [file,"temp" ++ file,"reverb","-w","10", "1", "100"] ""
  if code /= ExitSuccess then do
    exi <- doesFileExist $ "temp" ++ file
    if exi then removeFile ("temp" ++ file) >> error ("Composition.Sound.Faded.reverbFix: Operation on the file" ++ show file ++ " was unsucessful " ++ herr)
    else error $ "Composition.Sound.Faded.reverbFix: Operation on the file" ++ show file ++ " was unsucessful " ++ herr
  else renameFile ("temp" ++ file) file

