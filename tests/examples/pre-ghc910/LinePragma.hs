module UHC.Light.Compiler.Core.SysF.AsTy
( Ty
, ty2TySysfWithEnv, ty2TyC
, ty2TyCforFFI )
where
import UHC.Light.Compiler.Base.Common
import UHC.Light.Compiler.Opts.Base
import UHC.Light.Compiler.Error
import qualified UHC.Light.Compiler.Core as C
import qualified UHC.Light.Compiler.Ty as T
import UHC.Light.Compiler.FinalEnv

{-# LINE 50 "src/ehc/Core/SysF/AsTy.chs" #-}
-- | The type, represented by a term CExpr
type Ty             = C.SysfTy          -- base ty

-- | Binding the bound
type TyBind         = C.SysfTyBind
type TyBound        = C.SysfTyBound

-- | A sequence of parameters (for now just a single type)
type TySeq          = C.SysfTySeq


{-# LINE 67 "src/ehc/Core/SysF/AsTy.chs" #-}
ty2TySysfWithEnv :: ToSysfEnv -> T.Ty -> Ty
ty2TySysfWithEnv _   t =                                                     t

-- | Construct a type for use by AbstractCore
ty2TyC :: EHCOpts -> ToSysfEnv -> T.Ty -> C.CTy
ty2TyC o env t = C.mkCTy o t (ty2TySysfWithEnv env t)

{-# LINE 93 "src/ehc/Core/SysF/AsTy.chs" #-}
-- | Construct a type for use by AbstractCore, specifically for use by FFI
ty2TyCforFFI :: EHCOpts -> T.Ty -> C.CTy
ty2TyCforFFI o t = C.mkCTy o t                                t
