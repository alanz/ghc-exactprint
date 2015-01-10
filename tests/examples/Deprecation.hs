
module Deprecation
{-# Deprecated ["This is a module \"deprecation\"",
             "multi-line"] #-}
   ( foo )
 where

{-# DEPRECATEd   foo
         ["This is a multi-line",
          "deprecation message",
          "for foo"] #-}
foo :: Int
foo = 4

