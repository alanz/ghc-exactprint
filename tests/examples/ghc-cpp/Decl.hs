-- | Type checking declarations
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UnicodeSyntax #-}
module Statics.Decl (
  tcProg, tcDecls, tcDecl, tcSigExp,
) where

import Util
import qualified AST
import qualified Data.Loc
import Meta.Quasi
import Type
import Statics.Constraint
import Statics.Env as Env
import Statics.Error
import Statics.Type
import Statics.Expr
import Statics.Sealing

import Prelude ()
import Data.IORef (IORef)
import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Set  as S

-- | Type check a program
tcProg  ∷ MonadConstraint tv r m ⇒
          Γ tv → AST.Prog R → m (AST.Prog R, Maybe (Type tv))
tcProg γ prog0 = withLocation prog0 $ case prog0 of
  [prQ| $list:ds in $opt:me |]                  → do
    (ds', γ', _)        ← tcDecls [] γ ds
    meσ'                ← mapM (tcExpr γ') me
    let me' = fst <$> meσ'
    return ([prQ| $list:ds' in $opt:me' |], snd <$> meσ')

-- | Type check a declaration.
tcDecl  ∷ MonadConstraint tv r m ⇒
          [ModId] → Γ tv → AST.Decl R → m
          (AST.Decl R, Γ tv, Signature tv)
tcDecl μ γ d0 = withLocation d0 $ case d0 of
  [dc| let $π = $e |]                           → do
    (e', σs)    ← tcExprPatt γ e π
    γ'          ← γ !+! π -:*- σs
    return ([dc| let $π = $e' |], γ', zipWith SgVal (AST.dv π) σs)
  [dc| let rec $list:bns |]                     → do
    (bns', ns, σs) ← tcLetRecBindings γ bns
    γ'          ← γ !+! ns -:*- σs
    return ([dc| let rec $list:bns' |], γ', zipWith SgVal ns σs)
  [dc| type $tid:lhs = type $qtid:rhs |]        → do
    tc          ← γ !.! rhs
    let sig     = [SgTyp lhs tc { tcName = J (reverse μ) lhs }]
    return (d0, γ =+= sig, sig)
  [dc| abstype $list:at with $list:ds end |]    → do
    (sigC, sigA)        ← tcAbsTys μ γ at
    (ds', _, sig1)      ← tcDecls μ (γ =+= sigC) ds
    let sig             = sigA ++ replaceTyCons (getSigTyCons sigA) sig1
    γ'                  ← γ !+! sig
    return ([dc| abstype $list:at with $list:ds' end |], γ', sig)
  [dc| type $list:tds |]                        → do
    sig         ← tcTyDecs μ γ tds
    return (d0, γ =+= sig, sig)
  [dc| module type $sid:n = $sigexp |]          → do
    sig1        ← tcSigExp γ sigexp
    let sig     = [SgSig n sig1]
    return (d0, γ =+= sig, sig)
  [dc| module $mid:n = $modexp |]               → do
    (modexp', sig1)     ← tcModExp (n:μ) γ modexp
    let sig     = [SgMod n sig1]
    γ'          ← γ !+! sig
    return ([dc| module $mid:n = $modexp' |], γ', sig)
  [dc| open $modexp |]                          → do
    (modexp', sig) ← tcModExp μ γ modexp
    γ'          ← γ !+! sig
    return ([dc| open $modexp' |], γ', sig)
  [dc| local $list:ds0 with $list:ds1 end |]    → do
    (ds0', γ', _)    ← tcDecls (AST.ident "?LocalModule":μ) γ ds0
    (ds1', _,  sig1) ← tcDecls μ γ' ds1
    γ''              ← γ !+! sig1
    return ([dc| local $list:ds0' with $list:ds1' end |], γ'', sig1)
  [dc| exception $cid:c of $opt:mt |]           → do
    mσ ← toEmptyF <$$> mapM (tcType mempty γ) mt
    let sig     = [SgExn c mσ]
    return (d0, γ =+= sig, sig)
  [dc| $anti:a |]                               → $(AST.antifail)

-- | Type check a sequence of declarations
tcDecls ∷ MonadConstraint tv r m ⇒
          [ModId] → Γ tv → [AST.Decl R] →
          m ([AST.Decl R], Γ tv, Signature tv)
tcDecls _ γ []     = return ([], γ, [])
tcDecls μ γ (d:ds) = do
  (d', γ', sig0)   ← tcDecl μ γ d
  (ds', γ'', sig1) ← tcDecls μ γ' ds
  return (d':ds', γ'', sig0 ++ sig1)

-- | Type check a module expression
tcModExp ∷ MonadConstraint tv r m ⇒
           [ModId] → Γ tv → AST.ModExp R →
           m (AST.ModExp R, Signature tv)
tcModExp μ γ modexp0 = withLocation modexp0 $ case modexp0 of
  [meQ| struct $list:ds end |]                  → do
    (ds', _, sig)       ← tcDecls μ γ ds
    return ([meQ| struct $list:ds' end |], sig)
  [meQ| $qmid:n $list:_ |]                      → do
    (sig, _) ← γ !.! n
    return (modexp0, sig)
  [meQ| $modexp : $sigexp |]                    → do
    (modexp', sig0)     ← tcModExp μ γ modexp
    sig1                ← tcSigExp γ sigexp
    sig                 ← sealWith μ sig0 sig1
    return ([meQ| $modexp' : $sigexp |], sig)
  [meQ| $anti:a |]                              → $(AST.antifail)

-- | Type check a single signature item
tcSigItem ∷ MonadConstraint tv r m ⇒
            Γ tv → AST.SigItem R → m (Signature tv)
tcSigItem γ sigitem0 = withLocation sigitem0 $ case sigitem0 of
  [sgQ| val $vid:n : $t |]                      → do
    σ           ← tcType mempty γ t
    return [SgVal n σ]
  [sgQ| type $list:tds |]                       → tcTyDecs [] γ tds
  [sgQ| type $tid:lhs = type $qtid:rhs |]       → do
    tc          ← γ !.! rhs
    return [SgTyp lhs tc { tcName = J [] lhs }]
  [sgQ| module $mid:n : $sigexp |]              → do
    sig         ← tcSigExp γ sigexp
    return [SgMod n sig]
  [sgQ| module type $sid:n = $sigexp |]         → do
    sig         ← tcSigExp γ sigexp
    return [SgSig n sig]
  [sgQ| include $sigexp |]                      → tcSigExp γ sigexp
  [sgQ| exception $cid:c of $opt:mt |]          → do
    mσ ← toEmptyF <$$> mapM (tcType mempty γ) mt
    return [SgExn c mσ]
  [sgQ| $anti:a |]                              → $(AST.antifail)

-- | Type check a signature body
tcSigItems   ∷ MonadConstraint tv r m ⇒
               Γ tv → [AST.SigItem R] → m (Signature tv)
tcSigItems _ []       = return []
tcSigItems γ (sg:sgs) = do
  sig0  ← tcSigItem γ sg
  γ'    ← γ !+! sig0
  sig1  ← tcSigItems γ' sgs
  return (sig0 ++ sig1)

-- | Type check a signature expression
tcSigExp ∷ MonadConstraint tv r m ⇒
           Γ tv → AST.SigExp R → m (Signature tv)
tcSigExp γ sigexp0 = withLocation sigexp0 $ case sigexp0 of
  [seQ| sig $list:sgs end |]                    → tcSigItems γ sgs
  [seQ| $qsid:n $list:_ |]                      → fst <$> γ !.! n
  [seQ| $sigexp with type $list:αs $qtid:qc = $t |]
                                                → do
    sig         ← tcSigExp γ sigexp
    let td      = AST.tdSyn (jname qc) [(AST.tpVar <$> αs <*> pure 1, t)]
    [(_, tc)]   ← tcTyDecs' [] γ [td]
    return (fibrate qc tc sig)
  [seQ| $anti:a |]                              → $(AST.antifail)

-- | Type check the type declarations of an abstype block
tcAbsTys ∷ MonadConstraint tv r m ⇒
           [ModId] → Γ tv → [AST.AbsTy R] →
           m (Signature tv, Signature tv)
tcAbsTys μ γ ats = do
  (arities, quals, tydecs) ← unzip3 <$> mapM unAbsTy ats
  ntcs0                    ← tcTyDecs' μ γ tydecs
  ntcs1 ← sequence
    [ do
        qe ← indexQuals (AST.tdParams (view td)) qual
        let tc' = tc {
                    tcArity = arity,
                    tcQual  = qe,
                    tcCons  = mempty,
                    tcNext  = Nothing
                  }
        checkTyConMonotone (AST.tdParams (view td)) tc
        return (n, tc')
    | (n, tc) ← ntcs0
    | arity   ← arities
    | qual    ← quals
    | td      ← tydecs ]
  return (uncurry SgTyp <$> ntcs0, uncurry SgTyp <$> ntcs1)
  where
    unAbsTy (AST.N _ (AST.AbsTy arity qual td)) = return (arity, qual, td)
    unAbsTy (AST.N _ (AST.AbsTyAnti a))         = $(AST.antifail)

-- | Type check a type declaration group
tcTyDecs  ∷ MonadConstraint tv r m ⇒
            [ModId] → Γ tv → [AST.TyDec R] → m (Signature tv)
tcTyDecs  = uncurry SgTyp <$$$$$> tcTyDecs'

-- | Type check a type declaration group
tcTyDecs' ∷ MonadConstraint tv r m ⇒
            [ModId] → Γ tv → [AST.TyDec R] → m [(TypId, TyCon)]
tcTyDecs' μ γ tds = do
  stub_sig  ← forM tds $ \td → withLocation td $ case view td of
    AST.TdDat tid params _
      → allocStub tid (AST.tvqual <$> params)
    AST.TdSyn tid ((tps,_):_)
      → allocStub tid (Qa <$ tps)
    AST.TdAbs tid params variances guards qual
      → do
        qe ← indexQuals params qual
        ix ← tvUniqueID <$> newTV
        let tc = mkTC ix (J (reverse μ) tid)
                         qe
                         (zip3 variances
                               (AST.tvqual <$> params)
                               ((`elem` guards) <$> params))
        checkTyConMonotone params tc
        return (tid, tc)
    AST.TdSyn _ []
      → typeBug "tcTyDecs'" "Saw type synonym with 0 clauses."
    AST.TdAnti a
      → $(AST.antifail)
  real_sig ← iterChanging <-> stub_sig $ \sig →
    zipWithM (tcTyDec (γ =+= Env.fromList sig)) tds sig
  return (second (replaceTyCons (snd <$> real_sig)) <$> real_sig)
  where
    allocStub tid bounds = do
      ix ← tvUniqueID <$> newTV
      return (tid, mkTC ix (J (reverse μ) tid)
                           ((Omnivariant,,False) <$> bounds) ∷ TyCon)
    --

checkTyConMonotone ∷ MonadAlmsError m ⇒ [AST.TyVar R] → TyCon → m ()
checkTyConMonotone params tc = do
  let ftv_qe  = ftvSet (tcQual tc)
      bad_tvs = map ([msg| $2 (variance $1, at $3) |]
                       <$> sel2 <*> sel3 <*> AST.getLoc. sel3) .
                filter (\tup → S.member (sel1 tup) ftv_qe) .
                filter (\tup → sel2 tup ⊑ Contravariant) $
                zip3 [ 0 .. ] (tcArity tc) params
      name    = tcName tc
  unless (null bad_tvs) $
    typeError [msg| Type declaration for $q:name is inadmissable
      because it doesn’t satisfy the monotonicity condition for
      type constructors.
      All type variable parameters that appear in the qualifier of
      a type must be covariant or invariant, this is not satisfied
      by all parameters of $q:name:
      $ul:bad_tvs
    |]

tcTyDec ∷ MonadConstraint tv r m ⇒
          Γ tv → AST.TyDec R → (TypId, TyCon) → m (TypId, TyCon)
tcTyDec γ td (tid, tc) = withLocation td $ case view td of
  AST.TdDat _ params alts
    → do
      αs        ← mapM (curry newTV' Skolem) params
      let δ     = params -:*- αs
      mσs       ← mapM (mapM (tcType δ γ) . snd) alts
      let mσs'          = toEmptyF . closeTy 0 αs <$$> mσs
          arity         = M.findWithDefault 0 <-> ftvV mσs <$> αs
          bounds        = AST.tvqual <$> params
          guards        = M.findWithDefault True <-> ftvG mσs <$> αs
          qual          = case qualifierEnv [bounds] mσs' of
            QeA     → QeA
            QeU set → QeU (S.mapMonotonic each set)
              where each (Bound _ j _) = j
                    each (Free r)      = elimEmpty r
      when (arity  /= tcArity tc
         || bounds /= tcBounds tc
         || guards /= tcGuards tc
         || qual   /= tcQual tc)
        setChanged
      return (tid, tc {
                     tcArity  = arity,
                     tcBounds = bounds,
                     tcGuards = guards,
                     tcQual   = qual,
                     tcCons   = map fst alts -:*- mσs'
                   })
  AST.TdSyn _ cs@((tps0, _):_)
    → do
      let nparams = length tps0
      tassert (all ((== nparams) . length . fst) cs)
        [msg| In definition of type operator $q:tid, not all clauses
              have the same number of parameters. |]
      (cs', infos) ← unzip <$$> for cs $ \(tps, rhs) → do
        (tps', αss)     ← unzip <$> mapM (tcTyPat γ) tps
        αss'            ← mapM (mapM (const newTV . fst)) αss
        let (dot, nonDot)
                        = L.partition (snd . fst)
                            (zip (concat αss) (concat αss'))
            dot_αs      = first fst <$> dot
            αs          = fst . fst <$> nonDot
            αs'         = snd <$> nonDot
        σ               ← tcTypeRowDots (αs -:*- αs') dot_αs γ rhs
        qlss            ← mapM getTVBounds αss'
        let σ'          = toEmptyF (closeTy 0 (concat αss') σ)
            -- For each pattern, for each of its type variables,
            -- a triple of its variance, inclusion in the qualifer,
            -- and guardedness:
            kindses     = tyPatKinds <$> tps'
        -- Bounds are computed by checking which type variables need to
        -- be bounded for the right-hand side to be well-formed, then
        -- checking which are similarly bounded on the left.  For those
        -- that are not yet bounded on the left, if they are involved in
        -- the qualifier of the pattern then we can bound them by bounding
        -- the pattern, but otherwise it's an error.
        bounds  ← sequence
          [ bigMeet <$> sequence
            [ if qll ⊑ qlr then return Qa
              else if inQExp then return Qu
              else do
                typeError_ $ uncurry
                  [msg|
                    Ill-formed type $1 declaration.
                    <br>
                    Type variable $α must be bounded by U (unlimited)
                    for the type on the right-hand side of $2 to be
                    well-formed, but it is not bounded by its appearance
                    in the pattern, and because it does not contribute
                    to the qualifier of the pattern, bounded the pattern
                    cannot effectively bound $α.
                  |] $
                  if length cs == 1
                    then ("synonym", "the declaration")
                    else ("operator", "clause " ++ show i)
                return Qa
            | (_, inQExp, _, qll) ← kindsi
            | α   ← αsi
            | qlr ← qlsi ]
          | i      ← [1 ∷ Int .. ]
          | kindsi ← kindses
          | αsi    ← αss
          | qlsi   ← qlss ]
            -- The arity of each parameter is the join of the products
            -- of the arities of the type variables in the pattern and
            -- rhs type.
        let varmap      = ftvV σ
            arity       = [ bigJoin $
                              zipWith (*)
                                (sel1 <$> kindsi)
                                (M.findWithDefault 0 <-> varmap <$> αsi')
                          | kindsi ← kindses
                          | αsi'   ← αss' ]
            -- This is very permissive:
            guardmap    = ftvG σ
            guards      = [ all2 (||)
                              (M.findWithDefault True <-> guardmap <$> αsi')
                              (sel3 <$> kindsi)
                          | kindsi ← kindses
                          | αsi'   ← αss' ]
            -- For each parameter, a list of which of its type
            -- variables are significant to the qualifier
            qinvolveds = [ map snd . filter fst $
                              zip (sel2 <$> kindsi)
                                  αsi'
                          | kindsi ← kindses
                          | αsi'   ← αss' ]
            qual        = case qualifier σ of
              QeA       → QeA
              QeU βs    → bigJoin
               [ case L.findIndex (β `elem`) qinvolveds of
                   Nothing → QeA
                   Just ix
                     | Qu:_ ← drop ix bounds → qlitexp Qu
                     | otherwise             → qvarexp ix
               | Free β ← S.toList βs ]
        return ((tps', σ'), (arity, bounds, guards, qual))
      let (arities, boundses, guardses, quals) = unzip4 infos
          arity  = foldl1 (zipWith (⊔)) arities
          bounds = foldl1 (zipWith (⊓)) boundses
          guards = foldl1 (zipWith (&&)) guardses
          qual   = bigJoin quals
      when (arity  /= tcArity tc
         || bounds /= tcBounds tc
         || guards /= tcGuards tc
         || qual   /= tcQual tc)
        setChanged
      traceN 1 ("bounds", bounds)
      return (tid, tc {
                     tcArity  = arity,
                     tcBounds = bounds,
                     tcGuards = guards,
                     tcQual   = qual,
                     tcNext   = Just cs'
                   })
  AST.TdAbs _ _ _ _ _
    → return (tid, tc)
  AST.TdSyn _ []
    → typeBug "tcTyDec" "Saw type synonym with 0 clauses."
  AST.TdAnti a
    → $(AST.antifail)

-- | Convert a syntactic qualifier expression into an internal
--   qualifier expression over 'Int'.
indexQuals ∷ MonadAlmsError m ⇒
             [AST.TyVar R] → AST.QExp R → m (QExp Int)
indexQuals params = qInterpret resolver where
  resolver tv = case L.findIndex (== tv) params of
    Nothing → typeBug "indexQuals" "tv not found in type params"
    Just ix → return ix

-- | Given a functor, replace the contents with 'Empty', provided
--   there is no contents. If an 'Empty' value is actually required,
--   this is an error.
toEmptyF ∷ Functor f ⇒ f a → f Empty
toEmptyF = fmap toEmpty where
  toEmpty _ = throw (almsBug StaticsPhase "tcDecl" "saw free type variable")

---
--- MODULE SYSTEM
---

-- | Functional update on a signature
fibrate  ∷ QTypId → TyCon → Signature tv → Signature tv
fibrate (J [] tid) tc sig = map eachItem sig where
  eachItem (SgTyp tid' _)
    | tid == tid'       = SgTyp tid tc
  eachItem sigitem      = sigitem
fibrate (J (mid:rest) tid) tc sig = map eachItem sig where
  eachItem (SgMod mid' sig')
    | mid == mid'       = SgMod mid (fibrate (J rest tid) tc sig')
  eachItem sigitem      = sigitem

---
--- TESTING
---

test_tcProg ∷ AST.Prog R →
              IO (Either [AlmsError]
                         (Maybe (Type (TV IORef)),
                          ConstraintState (TV IORef) IORef))
test_tcProg p =
  runConstraintIO
    constraintState0
    (subst =<< snd <$> tcProg test_g0 p)

