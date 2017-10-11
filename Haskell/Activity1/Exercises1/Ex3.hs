--
-- Copyright (c) 2017 Daniel Gonçalves da Silva - https://github.com/danielgoncalvesti
-- GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
--
module Main where

mult5 :: Integer -> Bool
mult5 x = (x `rem` 5) == 0

main :: IO()
main = do
      print(mult5 5)