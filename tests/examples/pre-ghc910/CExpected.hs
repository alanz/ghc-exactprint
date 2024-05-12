module CExpected where
-- Test for refactor of if to case
-- The comments on the then and else legs should be preserved

foo x = case (odd x) of
          True  -> -- This is an odd result
            bob x 1
          False -> -- This is an even result
            bob x 2

bob x y = x + y

