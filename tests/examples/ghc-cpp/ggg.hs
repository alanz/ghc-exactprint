{-# LANGUAGE GHC_CPP             #-}

module TrailingContinue where

#define primArrayEltAs(ty,as)                                                 \
type instance DevicePtrs ty = CUDA.DevicePtr as ;                             \
type instance HostPtrs   ty = CUDA.HostPtr   as ;                             \

#define primArrayElt(ty) primArrayEltAs(ty,ty)

primArrayElt(Int)
primArrayElt(Int8)
primArrayElt(Int16)
primArrayElt(Int32)
primArrayElt(Int64)
