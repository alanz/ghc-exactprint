module CommentPlacement5 where

bar :: Foo -> ()
bar a =
  case a of
    GInt -> ()
    -- GBool -> ()
bar _ = ()

