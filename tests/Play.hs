

import Text.PrettyPrint


src =  text "-- | A simple let expression, to ensure the layout is detected"
    $$ text "-- With some haddock in the top"
    $$ text "{- And a normal\n   mulitline comment too -}"
    $$ (text "module" <+> text "Layout.LetExpr" <+> text "where")
    $$ text ""
    $$ text "-- comment"
    $$ text "foo" <+> text "="
        <+> (vcat [ text "let"
                     <+> vcat [text "x" <+> text "=" <+> text "1"
                              ,text "y" <+> text "=" <+> text "2"
                              ]
                  , text "in" <+> (text "x" <+> text "+" <+> text "y")
                  ]
            )

main = putStrLn $ render src

