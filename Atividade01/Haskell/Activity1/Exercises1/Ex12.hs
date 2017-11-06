-- Exercício 12: Dada a string “0123456789”, crie uma lista com os dígitos em formato Integer.
-- Copyright (c) 2017 Daniel Gonçalves da Silva - https://github.com/danielgoncalvesti
-- GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
--
module Main where

import Data.Char

main :: IO()
main = do 
      print (map digitToInt "0123456789")