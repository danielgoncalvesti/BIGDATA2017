--
-- Copyright (c) 2017 Daniel Gonçalves da Silva - https://github.com/danielgoncalvesti
-- GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
--
module Main where

mult35 :: Integer -> Bool
mult35 x = (x `rem` 3) == 0 || (x `rem` 5) == 0

main :: IO()
main = do
 print(mult35 3)
 print(mult35 5)