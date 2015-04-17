


foo =
  parseTestFile "gitlogo-double.ppm" "a multi-image file" $ do
      \res -> case res of
        Right ([ PPM { ppmHeader = h1 }
               , PPM { ppmHeader = h2 }], rest) -> do h1 `shouldBe` PPMHeader P6 220 92
                                                      h2 `shouldBe` PPMHeader P6 220 92
                                                      rest `shouldBe` Nothing
        Right r                                    -> assertFailure $ "parsed unexpected: " ++ show r
        Left e                                     -> assertFailure $ "did not parse: " ++ e
