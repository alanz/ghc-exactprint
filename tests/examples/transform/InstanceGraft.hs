module InstanceGraft where

class C t where
  go :: t -> Int
  go n = graft

instance C Int where
  go n = graft
