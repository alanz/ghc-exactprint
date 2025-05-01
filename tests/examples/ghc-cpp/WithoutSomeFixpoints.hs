-- Do not edit! Automatically created with doctest-extract from src/Combinatorics/Permutation/WithoutSomeFixpoints.hs
{-# LANGUAGE CPP #-}
{-# LINE 5 "src/Combinatorics/Permutation/WithoutSomeFixpoints.hs" #-}

module Test.Combinatorics.Permutation.WithoutSomeFixpoints where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 6 "src/Combinatorics/Permutation/WithoutSomeFixpoints.hs" #-}
import     qualified Combinatorics.Permutation.WithoutSomeFixpoints as PermWOFP
import     qualified Combinatorics as Comb
import     qualified Test.QuickCheck as QC
import     Control.Applicative ((<$>))
import     Data.List (nub)

genPermutationWOFP     :: QC.Gen (Int, String)
genPermutationWOFP     = do
       xs <- take 6 . nub <$> QC.arbitrary
       k <- QC.choose (0, length xs)
       return (k,xs)

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Combinatorics.Permutation.WithoutSomeFixpoints:34: "
{-# LINE 34 "src/Combinatorics/Permutation/WithoutSomeFixpoints.hs" #-}
 DocTest.property
{-# LINE 34 "src/Combinatorics/Permutation/WithoutSomeFixpoints.hs" #-}
     (QC.forAll genPermutationWOFP $ \(k,xs) -> PermWOFP.numbers !! length xs !! k == length (PermWOFP.enumerate k xs))
 DocTest.printPrefix "Combinatorics.Permutation.WithoutSomeFixpoints:35: "
{-# LINE 35 "src/Combinatorics/Permutation/WithoutSomeFixpoints.hs" #-}
 DocTest.property
{-# LINE 35 "src/Combinatorics/Permutation/WithoutSomeFixpoints.hs" #-}
     (QC.forAll (QC.choose (0,100)) $ \k -> Comb.factorial (toInteger k) == PermWOFP.numbers !! k !! 0)
 DocTest.printPrefix "Combinatorics.Permutation.WithoutSomeFixpoints:36: "
{-# LINE 36 "src/Combinatorics/Permutation/WithoutSomeFixpoints.hs" #-}
 DocTest.property
{-# LINE 36 "src/Combinatorics/Permutation/WithoutSomeFixpoints.hs" #-}
     (QC.forAll (QC.choose (0,100)) $ \k -> Comb.derangementNumber (toInteger k) == PermWOFP.numbers !! k !! k)

