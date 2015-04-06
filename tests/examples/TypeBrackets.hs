
instance (CommandFunction r st,Completion compl st)
      => CommandFunction (Completable compl -> r) st where

  parseCommand wbc =
     ( doParseCommand
          (Just (OtherCompleter (complete (undefined::compl))))
          (wordRegex wbc)
          Completable
          wbc
     ) :: (Completable compl -> r) -> CommandParser st

  commandSyntax (f:: (Completable compl -> r)) =
     text (completableLabel (undefined::compl)) : commandSyntax (f undefined)
