{-# LANGUAGE QuasiQuotes, TypeFamilies, PackageImports #-}

module Text.Markdown.Pap.Parser (
    parseMrd
) where

import Control.Arrow
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Data.Maybe
import Data.Char
import Text.Papillon

import Text.Markdown.Pap.Text

parseMrd :: String -> Maybe [Text]
parseMrd src = case flip runState (0, [- 1]) $ runErrorT $ markdown $ parse src of
    (Right (r, _), _) -> Just r
    _ -> Nothing

clear :: State (Int, [Int]) Bool
clear = put (0, [- 1]) >> return True

reset :: State (Int, [Int]) Bool
reset = modify (first $ const 0) >> return True

count :: State (Int, [Int]) ()
count = modify $ first (+ 1)

deeper :: State (Int, [Int]) Bool
deeper = do
    (n, n0 : ns) <- get
    if n > n0 then put (n, n : n0 : ns) >> return True else return False

same :: State (Int, [Int]) Bool
same = do
    (n, n0 : _) <- get
    return $ n == n0

shallow :: State (Int, [Int]) Bool
shallow = do
    (n, n0 : ns) <- get
    if n < n0 then put (n, ns) >> return True else return False

[papillon|

monad: State (Int, [Int])

markdown :: [Text]
    = md:(m:markdown1 _:dmmy[clear] { return m })*      { return md }

markdown1 :: Text
    = h:header              { return h }
    / l:link '\n'*              { return l }
    / i:image '\n'*             { return i }
    / l:list '\n'*              { return $ List l }
    / c:code                { return $ Code c }
    / p:paras               { return $ Paras p }

header :: Text
    = n:sharps _:<isSpace>* l:line '\n'+    { return $ Header n l }
    / l:line '\n' _:equals '\n'+        { return $ Header 1 l }
    / l:line '\n' _:hyphens '\n'+       { return $ Header 2 l }

sharps :: Int
    = '#' n:sharps              { return $ n + 1 }
    / '#'                   { return 1 }

equals :: ()
    = '=' _:equals
    / '='

hyphens :: ()
    = '-' _:hyphens
    / '-'

line :: String
    = l:<(`notElem` "#\n")>+        { return l }

line' :: String
    = l:<(`notElem` "\n")>+         { return l }

code :: String
    = l:fourSpacesLine c:code       { return $ l ++ c }
    / l:fourSpacesLine          { return l }

fourSpacesLine :: String
    = _:fourSpaces l:line' ns:('\n' { return '\n' })+   { return $ l ++ ns }

fourSpaces :: ()
    = ' ' ' ' ' ' ' '

list :: List = _:cnt _:dmmy[deeper] l:list1 ls:list1'* _:shllw  { return $ l : ls }

cnt :: () = _:dmmy[reset] _:(' ' { count })*

list1' :: List1
    = _:cnt _:dmmy[same] l:list1        { return l }

list1 :: List1
    = _:listHead ' ' l:line '\n' ls:list?
        { return $ BulItem l $ fromMaybe [] ls }
    / _:nListHead ' ' l:line '\n' ls:list?
        { return $ OrdItem l $ fromMaybe [] ls }

listHead :: ()
    = '*' / '-' / '+'

nListHead :: ()
    = _:<isDigit>+ '.'

paras :: [String]
    = ps:para+              { return ps }

para :: String
    = ls:(!_:('!') !_:listHead !_:nListHead !_:header !_:fourSpaces l:line '\n' { return l })+ _:('\n' / !_ / !_:para)
                        { return $ unwords ls }

shllw :: ()
    = _:dmmy[shallow]
    / !_
    / !_:list

dmmy :: () =

link :: Text
    = '[' t:<(/= ']')>+ ']' ' '* '(' a:<(/= ')')>+ ')' { return $ Link t a "" }

image :: Text
    = '!' '[' alt:<(/= ']')>+ ']' ' '* '(' addrs:<(`notElem` ")\" ")>+ ' '*
        '"' t:<(/= '"')>+ '"' ')'
        { return $ Image alt addrs t }

|]
