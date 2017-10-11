-- Exercício 08: Crie uma lista de anos bissextos desde o ano 1 até o atual.
-- Copyright (c) 2017 Daniel Gonçalves da Silva - https://github.com/danielgoncalvesti
-- GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
--
module Main where

isLeap :: Integer -> Bool
isLeap year 
  | year `rem` 400 == 0 = True
  | year `rem` 100 == 0 = False
  | year `rem`   4 == 0 = True
  | otherwise           = False
  

main :: IO()

main = do
    let years = [1..2017]
    let lista = [x | x <- years, isLeap x]
    print(lista)
