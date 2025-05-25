-- From ./hackage-roundtrip-work/fourmolu-0.18.0.0/data/examples/declaration/default/default-out.hs

module MyModule (default Monoid) where

default (Int, Foo, Bar)

default
  ( Int,
    Foo,
    Bar
  )

default Num (Int, Float)

default IsList ([], Vector)

default IsString (Text.Text, Foundation.String, String)
