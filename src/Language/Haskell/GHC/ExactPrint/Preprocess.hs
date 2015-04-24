module Language.Haskell.GHC.ExactPrint.Preprocess
   (
     stripLinePragmas
   , getCppTokensAsComments
   , getPreprocessedSrcDirect
     -- AZ's baggage
   , ghead,glast,gtail,gfromJust
   ) where

import qualified Bag            as GHC
import qualified DriverPipeline as GHC
import qualified DynFlags       as GHC
import qualified ErrUtils       as GHC
import qualified FastString     as GHC
import qualified GHC            as GHC hiding (parseModule)
import qualified HscTypes       as GHC
import qualified Lexer          as GHC
import qualified MonadUtils     as GHC
import qualified SrcLoc         as GHC
import qualified StringBuffer   as GHC

import Control.Exception
import Data.List hiding (find)
import Data.Maybe
import Language.Haskell.GHC.ExactPrint.Delta
import Language.Haskell.GHC.ExactPrint.GhcInterim
import Language.Haskell.GHC.ExactPrint.Types
import qualified Data.Set as Set
import qualified Data.Text.IO as T
import qualified Data.Text as T

-- import Debug.Trace

-- ---------------------------------------------------------------------

stripLinePragmas :: String -> (String, [Comment])
stripLinePragmas = unlines' . unzip . findLines . lines
  where
    unlines' (a, b) = (unlines a, catMaybes b)

findLines :: [String] -> [(String, Maybe Comment)]
findLines = zipWith checkLine [1..]

checkLine :: Int -> String -> (String, Maybe Comment)
checkLine line s
  |  "{-# LINE" `isPrefixOf` s =
       let (pragma, res) = getPragma s
           size   = length pragma
       in (res, Just $ Comment ((line, 1), (line, size+1)) pragma)
  -- Deal with shebang/cpp directives too
  |  "#" `isPrefixOf` s = ("",Just $ Comment ((line, 1), (line, length s)) s)
  | otherwise = (s, Nothing)

getPragma :: String -> (String, String)
getPragma [] = error "Input must not be empty"
getPragma s@(x:xs)
  | "#-}" `isPrefixOf` s = ("#-}", "   " ++ drop 3 s)
  | otherwise =
      let (prag, remline) = getPragma xs
      in (x:prag, ' ':remline)

-- ---------------------------------------------------------------------

-- | Replacement for original 'getRichTokenStream' which will return
-- the tokens for a file processed by CPP.
-- See bug <http://ghc.haskell.org/trac/ghc/ticket/8265>
getCppTokensAsComments :: GHC.GhcMonad m => GHC.DynFlags -> FilePath -> m [Comment]
getCppTokensAsComments flags sourceFile = do
  source <- GHC.liftIO $ GHC.hGetStringBuffer sourceFile
  let startLoc = GHC.mkRealSrcLoc (GHC.mkFastString sourceFile) 1 1
  case GHC.lexTokenStream source startLoc flags of
    GHC.POk _ _ts -> return []
    GHC.PFailed _span _err ->
        do
           (_,_txt,strSrcBuf,flags2) <- getPreprocessedSrcDirect sourceFile
           case GHC.lexTokenStream strSrcBuf startLoc flags2 of
             GHC.POk _ ts ->
               do directiveToks <- GHC.liftIO $ getPreprocessorAsComments sourceFile
                  nonDirectiveToks <- tokeniseOriginalSrc startLoc flags2 source
                  let toks = GHC.addSourceToTokens startLoc source ts
                  let cppCommentToks = getCppTokens directiveToks nonDirectiveToks toks
                  return $ map (tokComment . commentToAnnotation . fst) cppCommentToks
             GHC.PFailed sspan err -> parseError flags2 sspan err

-- ---------------------------------------------------------------------

-- | Combine the three sets of tokens to produce a single set that
-- represents the code compiled, and will regenerate the original
-- source file.
-- [@directiveToks@] are the tokens corresponding to preprocessor
--                   directives, converted to comments
-- [@origSrcToks@] are the tokenised source of the original code, with
--                 the preprocessor directives stripped out so that
--                 the lexer  does not complain
-- [@postCppToks@] are the tokens that the compiler saw originally
-- NOTE: this scheme will only work for cpp in -nomacro mode
getCppTokens ::
     [(GHC.Located GHC.Token, String)]
  -> [(GHC.Located GHC.Token, String)]
  -> [(GHC.Located GHC.Token, String)]
  -> [(GHC.Located GHC.Token, String)]
getCppTokens directiveToks origSrcToks postCppToks = toks
  where
    locFn (GHC.L l1 _,_) (GHC.L l2 _,_) = compare l1 l2
    m1Toks = mergeBy locFn postCppToks directiveToks

    -- We must now find the set of tokens that are in origSrcToks, but
    -- not in m1Toks

    -- GHC.Token does not have Ord, can't use a set directly
    origSpans = map (\(GHC.L l _,_) -> l) origSrcToks
    m1Spans = map (\(GHC.L l _,_) -> l) m1Toks
    missingSpans = (Set.fromList origSpans) Set.\\ (Set.fromList m1Spans)

    missingToks = filter (\(GHC.L l _,_) -> Set.member l missingSpans) origSrcToks

    missingAsComments = map mkCommentTok missingToks
      where
        mkCommentTok :: (GHC.Located GHC.Token,String) -> (GHC.Located GHC.Token,String)
        mkCommentTok (GHC.L l _,s) = (GHC.L l (GHC.ITlineComment s),s)

    toks = mergeBy locFn directiveToks missingAsComments

-- ---------------------------------------------------------------------

tokeniseOriginalSrc ::
  GHC.GhcMonad m
  => GHC.RealSrcLoc -> GHC.DynFlags -> GHC.StringBuffer
  -> m [(GHC.Located GHC.Token, String)]
tokeniseOriginalSrc startLoc flags buf = do
  let src = stripPreprocessorDirectives buf
  case GHC.lexTokenStream src startLoc flags of
    GHC.POk _ ts -> return $ GHC.addSourceToTokens startLoc src ts
    GHC.PFailed sspan err -> parseError flags sspan err

-- ---------------------------------------------------------------------

-- | Strip out the CPP directives so that the balance of the source
-- can tokenised.
stripPreprocessorDirectives :: GHC.StringBuffer -> GHC.StringBuffer
stripPreprocessorDirectives buf = buf'
  where
    srcByLine = lines $ sbufToString buf
    noDirectivesLines = map (\line -> if line /= [] && head line == '#' then "" else line) srcByLine
    buf' = GHC.stringToStringBuffer $ unlines noDirectivesLines

-- ---------------------------------------------------------------------

sbufToString :: GHC.StringBuffer -> String
sbufToString sb@(GHC.StringBuffer _buf len _cur) = GHC.lexemeToString sb len

-- ---------------------------------------------------------------------

getPreprocessedSrcDirect :: (GHC.GhcMonad m) => FilePath -> m (String, T.Text, GHC.StringBuffer, GHC.DynFlags)
getPreprocessedSrcDirect src_fn = do
  hsc_env <- GHC.getSession
  (dflags', hspp_fn) <- GHC.liftIO $ GHC.preprocess hsc_env (src_fn, Nothing)
  buf <- GHC.liftIO $ GHC.hGetStringBuffer hspp_fn
  txt <- GHC.liftIO $ T.readFile hspp_fn
  return (hspp_fn, txt, buf, dflags')

-- ---------------------------------------------------------------------

-- | Get the preprocessor directives as comment tokens from the
-- source.
getPreprocessorAsComments :: FilePath -> IO [(GHC.Located GHC.Token, String)]
getPreprocessorAsComments srcFile = do
  fcontents <- readFile srcFile
  let directives = filter (\(_lineNum,line) -> line /= [] && head line == '#') $ zip [1..] $ lines fcontents

  let mkTok (lineNum,line) = (GHC.L l (GHC.ITlineComment line),line)
       where
         start = GHC.mkSrcLoc (GHC.mkFastString srcFile) lineNum 1
         end   = GHC.mkSrcLoc (GHC.mkFastString srcFile) lineNum (length line)
         l = GHC.mkSrcSpan start end

  let toks = map mkTok directives
  return toks

-- ---------------------------------------------------------------------

parseError :: GHC.DynFlags -> GHC.SrcSpan -> GHC.MsgDoc -> m b
parseError dflags sspan err = do
     throw $ GHC.mkSrcErr (GHC.unitBag $ GHC.mkPlainErrMsg dflags sspan err)

-- ---------------------------------------------------------------------

-- Copied over from MissingH, the dependency cause travis to fail

{- | Merge two sorted lists using into a single, sorted whole,
allowing the programmer to specify the comparison function.

QuickCheck test property:

prop_mergeBy xs ys =
    mergeBy cmp (sortBy cmp xs) (sortBy cmp ys) == sortBy cmp (xs ++ ys)
          where types = xs :: [ (Int, Int) ]
                cmp (x1,_) (x2,_) = compare x1 x2
-}
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy _cmp [] ys = ys
mergeBy _cmp xs [] = xs
mergeBy cmp (allx@(x:xs)) (ally@(y:ys))
        -- Ordering derives Eq, Ord, so the comparison below is valid.
        -- Explanation left as an exercise for the reader.
        -- Someone please put this code out of its misery.
    | (x `cmp` y) <= EQ = x : mergeBy cmp xs ally
    | otherwise = y : mergeBy cmp allx ys



-- ---------------------------------------------------------------------
-- Putting these here for the time being, to avoid import loops

ghead :: String -> [a] -> a
ghead  info []    = error $ "ghead "++info++" []"
ghead _info (h:_) = h

glast :: String -> [a] -> a
glast  info []    = error $ "glast " ++ info ++ " []"
glast _info h     = last h

gtail :: String -> [a] -> [a]
gtail  info []   = error $ "gtail " ++ info ++ " []"
gtail _info h    = tail h

gfromJust :: String -> Maybe a -> a
gfromJust _info (Just h) = h
gfromJust  info Nothing = error $ "gfromJust " ++ info ++ " Nothing"
