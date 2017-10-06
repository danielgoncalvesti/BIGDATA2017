--
-- Copyright (c) 2017 Daniel Gonçalves da Silva - https://github.com/danielgoncalvesti
-- GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
--
module Main where

import System.Environment

verifyValue :: Integer -> Bool
verifyValue x | x < -1 || x == 2 = True
              | otherwise = False

-- | The main function
main :: IO ()
main = do
  print(verifyValue (-2))