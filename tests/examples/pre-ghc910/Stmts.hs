module Stmts where

-- Make sure we get all the semicolons in statements
;;;;  ;;
import Data.List
; ; ;
import Data.Maybe
   ; ;;
foo :: IO ()
foo = do
  do { ;;;; a }
  a
; ;;
bar :: IO ()
bar = do
  { ;  ;
    a ;;
    b
  }
 ; ;;  ;
baz :: IO ()
baz = do { ;; s ; s ; ; s ;; }
;
a = undefined
b = undefined
s = undefined
