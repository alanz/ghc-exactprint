{-

The original contents of this module were from Retrie.ExactPrint,
Retrie.Fixity and from haskell-src-exts

-}
{-# LANGUAGE RankNTypes #-}
module Language.Haskell.GHC.ExactPrint.Fixity
  ( FixityEnv
  , mkFixityEnv
  , lookupOp
  , lookupOpRdrName
  , Fixity(..)
  , FixityDirection(..)
  , extendFixityEnv
  , ppFixityEnv

  -- * ghc-exactprint usage
  , fix
  , defaultFixityEnv
  ) where

import GHC
import GHC.Data.FastString
import GHC.Data.FastString.Env
import GHC.Types.Fixity
import GHC.Types.Name.Occurrence
import GHC.Types.SourceText
import GHC.Types.Unique.FM

import Control.Monad
import Control.Monad.IO.Class
-- import Control.Monad.Trans

import Data.Data hiding (Fixity)
import Data.Generics hiding (Fixity)

import Language.Haskell.GHC.ExactPrint.Transform
import Language.Haskell.GHC.ExactPrint.Utils

-- ---------------------------------------------------------------------

newtype FixityEnv = FixityEnv
  { unFixityEnv :: FastStringEnv (FastString, Fixity) }

instance Semigroup FixityEnv where
  -- | 'mappend' for 'FixityEnv' is right-biased
  -- (<>) = mappend
  (<>) (FixityEnv e1) (FixityEnv e2) = FixityEnv (plusFsEnv e1 e2)

instance Monoid FixityEnv where
  mempty = mkFixityEnv []
  -- | 'mappend' for 'FixityEnv' is right-biased
  -- mappend (FixityEnv e1) (FixityEnv e2) = FixityEnv (plusFsEnv e1 e2)

lookupOp :: LHsExpr GhcPs -> FixityEnv -> Fixity
lookupOp (L _ e) | Just n <- varRdrName e = lookupOpRdrName n
lookupOp _ = error "lookupOp: called with non-variable!"

lookupOpRdrName :: LocatedN RdrName -> FixityEnv -> Fixity
lookupOpRdrName (L _ n) (FixityEnv env) =
  maybe defaultFixity snd $ lookupFsEnv env (occNameFS $ occName n)

mkFixityEnv :: [(FastString, (FastString, Fixity))] -> FixityEnv
mkFixityEnv = FixityEnv . mkFsEnv

extendFixityEnv :: [(FastString, Fixity)] -> FixityEnv -> FixityEnv
extendFixityEnv l (FixityEnv env) =
  FixityEnv $ extendFsEnvList env [ (fs, p) | p@(fs,_) <- l ]

ppFixityEnv :: FixityEnv -> String
ppFixityEnv = unlines . map ppFixity . eltsUFM . unFixityEnv
  where
    ppFixity (fs, Fixity _ p d) = unwords
      [ case d of
          InfixN -> "infix"
          InfixL -> "infixl"
          InfixR -> "infixr"
      , show p
      , unpackFS fs
      ]

-- Fixity traversal -----------------------------------------------------------

-- | Re-associate AST using given 'FixityEnv'. (The GHC parser has no knowledge
-- of operator fixity, because that requires running the renamer, so it parses
-- all operators as left-associated.)
fix :: (Data ast, MonadIO m) => FixityEnv -> ast -> TransformT m ast
fix env = fixAssociativity >=> fixEntryDP
  where
    fixAssociativity = everywhereM (mkM (fixOneExpr env) `extM` fixOnePat env)
    fixEntryDP = everywhereM (mkM fixOneEntryExpr `extM` fixOneEntryPat)

-- Should (x op1 y) op2 z be reassociated as x op1 (y op2 z)?
associatesRight :: Fixity -> Fixity -> Bool
associatesRight (Fixity _ p1 a1) (Fixity _ p2 _a2) =
  p2 > p1 || p1 == p2 && a1 == InfixR

-- We know GHC produces left-associated chains, so 'z' is never an
-- operator application. We also know that this will be applied bottom-up
-- by 'everywhere', so we can assume the children are already fixed.
fixOneExpr
  :: MonadIO m
  => FixityEnv
  -> LHsExpr GhcPs
  -> TransformT m (LHsExpr GhcPs)
fixOneExpr env (L l2 (OpApp x2 ap1@(L l1 (OpApp x1 x op1 y)) op2 z))
  | associatesRight (lookupOp op1 env) (lookupOp op2 env) = do
    -- lift $ liftIO $ debugPrint Loud "fixOneExpr:(l1,l2)="  [showAst (l1,l2)]
    let ap2' = L (stripComments l2) $ OpApp x2 y op2 z
    (ap1_0, ap2'_0) <- swapEntryDPT ap1 ap2'
    ap1_1 <- transferAnnsT isComma ap2'_0 ap1_0
    -- lift $ liftIO $ debugPrint Loud "fixOneExpr:recursing"  []
    rhs <- fixOneExpr env ap2'_0
    let res = L l2 $ OpApp x1 x op1 rhs
    -- lift $ liftIO $ debugPrint Loud "fixOneExpr:returning:str"  [printNoLeadingSpaces res]
    -- lift $ liftIO $ debugPrint Loud "fixOneExpr:returning:ast\n"  [showAst res]
    -- return $ L l2 $ OpApp x1 x op1 rhs
    return res
fixOneExpr _ e = return e

fixOnePat :: Monad m => FixityEnv -> LPat GhcPs -> TransformT m (LPat GhcPs)
fixOnePat env (L l2 (ConPat x2 op2 (InfixCon ap1@(L l1 (ConPat x1 op1 (InfixCon x y))) z)))
  | associatesRight (lookupOpRdrName op1 env) (lookupOpRdrName op2 env) = do
    let ap2' = L l2 (ConPat x2 op2 (InfixCon y z))
    (ap1_0, ap2'_0) <- swapEntryDPT ap1 ap2'
    ap1_1 <- transferAnnsT isComma ap2' ap1
    rhs <- fixOnePat env ap2'_0
    return $ L l1 (ConPat x1 op1 (InfixCon x rhs))
fixOnePat _ e = return e

-- ---------------------------------------------------------------------

fixOneEntryExpr :: MonadIO m => LHsExpr GhcPs -> TransformT m (LHsExpr GhcPs)
fixOneEntryExpr e@(L l (OpApp a x b c)) = do
  -- lift $ liftIO $ debugPrint Loud "fixOneEntryExpr:orig="  [printNoLeadingSpaces e]
  -- lift $ liftIO $ debugPrint Loud "fixOneEntryExpr:(e,x)="  [showAst (e,x)]
  -- lift $ liftIO $ debugPrint Loud "fixOneEntryExpr:e="  [showAst e]
  (e',x') <- fixOneEntry e x
  let repl = (L (getLoc e') (OpApp a x' b c))
  -- lift $ liftIO $ debugPrint Loud "fixOneEntryExpr:(e',x')="  [showAst (e',x')]
  -- lift $ liftIO $ debugPrint Loud "fixOneEntryExpr:returning="  [showAst repl]
  -- lift $ liftIO $ debugPrint Loud "fixOneEntryExpr:returning:str="  [printNoLeadingSpaces repl]
  return (L (getLoc e') (OpApp a x' b c))
fixOneEntryExpr e = return e

fixOneEntryPat :: MonadIO m => LPat GhcPs -> TransformT m (LPat GhcPs)
fixOneEntryPat p@(L l (ConPat a b (InfixCon x c))) = do
    (p', x') <- fixOneEntry p x
    return (L (getLoc p') (ConPat a b (InfixCon x' c)))
fixOneEntryPat pat = return pat

-- Move leading whitespace from the left child of an operator application
-- to the application itself. We need this so we have correct offsets when
-- substituting into patterns and don't end up with extra leading spaces.
-- We can assume it is run bottom-up, and that precedence is already fixed.
fixOneEntry
  :: (MonadIO m)
  => LocatedA a -- ^ Overall application
  -> LocatedA a -- ^ Left child
  -> TransformT m (LocatedA a, LocatedA a)
fixOneEntry e x = do
  -- lift $ liftIO $ debugPrint Loud "fixOneEntry:(e,x)="  [showAst (e,x)]
  -- -- anns <- getAnnsT
  -- let
  --   zeros = SameLine 0
  --   (xdp, ard) =
  --     case M.lookup (mkAnnKey x) anns of
  --       Nothing -> (zeros, zeros)
  --       Just ann -> (annLeadingCommentEntryDelta ann, annEntryDelta ann)
  --   xr = getDeltaLine xdp
  --   xc = deltaColumn xdp
  --   actualRow = getDeltaLine ard
  --   edp =
  --     maybe zeros annLeadingCommentEntryDelta $ M.lookup (mkAnnKey e) anns
  --   er = getDeltaLine edp
  --   ec = deltaColumn edp
  -- when (actualRow == 0) $ do
  --   setEntryDPT e $ deltaPos (er, xc + ec)
  --   setEntryDPT x $ deltaPos (xr, 0)

  -- We assume that ghc-exactprint has converted all Anchor's to use their delta variants.
  -- Get the dp for the x component
  let xdp = entryDP x
  let xr = getDeltaLine xdp
  let xc = deltaColumn xdp
  -- Get the dp for the e component
  let edp = entryDP e
  let er = getDeltaLine edp
  let ec = deltaColumn edp
  case xdp of
    SameLine n -> do
      -- lift $ liftIO $ debugPrint Loud "fixOneEntry:(xdp,edp)="  [showAst (xdp,edp)]
      -- lift $ liftIO $ debugPrint Loud "fixOneEntry:(dpx,dpe)="  [showAst ((deltaPos er (xc + ec)),(deltaPos xr 0))]
      -- lift $ liftIO $ debugPrint Loud "fixOneEntry:e'="  [showAst e]
      -- lift $ liftIO $ debugPrint Loud "fixOneEntry:e'="  [showAst (setEntryDP e (deltaPos er (xc + ec)))]
      return ( setEntryDP e (deltaPos er (xc + ec))
             , setEntryDP x (deltaPos xr 0))
    _ -> return (e,x)

  -- anns <- getAnnsT
  -- let
  --   zeros = DP (0,0)
  --   (DP (xr,xc), DP (actualRow,_)) =
  --     case M.lookup (mkAnnKey x) anns of
  --       Nothing -> (zeros, zeros)
  --       Just ann -> (annLeadingCommentEntryDelta ann, annEntryDelta ann)
  --   DP (er,ec) =
  --     maybe zeros annLeadingCommentEntryDelta $ M.lookup (mkAnnKey e) anns
  -- when (actualRow == 0) $ do
  --   setEntryDPT e $ DP (er, xc + ec)
  --   setEntryDPT x $ DP (xr, 0)
  -- return e


-- ---------------------------------------------------------------------

-- Fixity table from haskell-src-exts
-- https://hackage.haskell.org/package/haskell-src-exts-1.23.1/docs/src/Language.Haskell.Exts.Fixity.html#baseFixities

defaultFixityEnv :: FixityEnv
-- defaultFixityEnv = mkFixityEnv $ map hseToGHC HSE.baseFixities
defaultFixityEnv = mkFixityEnv baseFixities

-- hseToGHC :: HSE.Fixity -> (FastString, (FastString, Fixity))
-- hseToGHC (HSE.Fixity assoc p nm) =
--   (fs, (fs, Fixity (SourceText nm') p (dir assoc)))
--   where
--     dir (HSE.AssocNone _)  = InfixN
--     dir (HSE.AssocLeft _)  = InfixL
--     dir (HSE.AssocRight _) = InfixR

--     nm' = case nm of
--       HSE.Qual _ _ n -> nameStr n
--       HSE.UnQual _ n -> nameStr n
--       _             -> "SpecialCon"

--     fs = mkFastString nm'

--     nameStr (HSE.Ident _ s)  = s
--     nameStr (HSE.Symbol _ s) = s

infixr__, infixl__, infix__ :: Int -> [String] -> [(FastString, (FastString, Fixity))]
infixr__ = fixity_  InfixR
infixl__ = fixity_  InfixL
infix__  = fixity_  InfixN

fixity_ :: FixityDirection -> Int -> [String] -> [(FastString, (FastString, Fixity))]
fixity_ a p = map go
    where
        go n = (fs, (fs, Fixity (SourceText nm') p a))
          where
            fs = mkFastString nm'
            nm' = op n

        op ('`':xs) = init xs
        op xs = xs

-- infixr_, infixl_, infix_ :: Int -> [String] -> [Fixity]
-- infixr_ = fixity  assocRight
-- infixl_ = fixity  assocLeft
-- infix_  = fixity  assocNone

-- -- Internal: help function for the above definitions.
-- fixity :: Assoc () -> Int -> [String] -> [Fixity]
-- fixity a p = map (Fixity a p . op)
--     where
--         op ('`':xs) = UnQual () $ Ident () $ init xs
--         op xs = UnQual () $ Symbol () xs



-- -- | All fixities defined in the Prelude.
-- preludeFixities :: [Fixity]
preludeFixities :: [(FastString, (FastString, Fixity))]
preludeFixities = concat
    [infixr__ 9  ["."]
    ,infixl__ 9  ["!!"]
    ,infixr__ 8  ["^","^^","**"]
    ,infixl__ 7  ["*","/","`quot`","`rem`","`div`","`mod`"]
    ,infixl__ 6  ["+","-"]
    ,infixr__ 5  [":","++"]
    ,infix__  4  ["==","/=","<","<=",">=",">","`elem`","`notElem`"]
    ,infixl__ 4  ["<$>","<$","<*>","<*","*>"]
    ,infixr__ 3  ["&&"]
    ,infixr__ 2  ["||"]
    ,infixl__ 1  [">>",">>="]
    ,infixr__ 1  ["=<<"]
    ,infixr__ 0  ["$","$!","`seq`"]
    ]

-- | All fixities defined in the base package.
--
--   Note that the @+++@ operator appears in both Control.Arrows and
--   Text.ParserCombinators.ReadP. The listed precedence for @+++@ in
--   this list is that of Control.Arrows.
baseFixities :: [(FastString, (FastString, Fixity))]
baseFixities = preludeFixities ++ concat
    [infixl__ 9 ["!","//","!:"]
    ,infixr__ 9 ["`Compose`"]
    ,infixl__ 8 ["`shift`","`rotate`","`shiftL`","`shiftR`","`rotateL`","`rotateR`"]
    ,infixl__ 7 [".&.","%"]
    ,infixr__ 6 ["<>"]
    ,infixl__ 6 ["`xor`"]
    ,infix__  6 [":+"]
    ,infixl__ 5 [".|."]
    ,infixr__ 5 ["+:+","<++","<+>","<|"] -- fixity conflict for +++ between ReadP and Arrow
    ,infix__  5 ["\\\\"]
    ,infixl__ 4 ["<**>","$>","<$","<$!>"]
    ,infix__  4 ["`elemP`","`notElemP`",":~:",":~~:"]
    ,infixl__ 3 ["<|>"]
    ,infixr__ 3 ["&&&","***"]
    ,infixr__ 2 ["+++","|||"]
    ,infixr__ 1 ["<=<",">=>",">>>","<<<","^<<","<<^","^>>",">>^"]
    ,infixl__ 1 ["&"]
    ,infixl__ 0 ["`on`"]
    ,infixr__ 0 ["`par`","`pseq`"]
    ]

