--
-- Copyright (c) 2017 Daniel Gonçalves da Silva - https://github.com/danielgoncalvesti
-- GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
--
module Main where

div2d :: Integer -> Double
div2d x = x / 2

main :: IO()
main = do
	print(div2d 2)
