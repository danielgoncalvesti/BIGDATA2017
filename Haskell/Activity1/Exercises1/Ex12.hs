-- Exercício 12: Dada a string “0123456789”, crie uma lista com os dígitos em formato Integer.
-- Copyright (c) 2017 Daniel Gonçalves da Silva - https://github.com/danielgoncalvesti
-- GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
--
module Main where

main :: IO()
main = do 
      let string1 = "0123456789"
      print (map (read . (:"")) string1 :: [Int])