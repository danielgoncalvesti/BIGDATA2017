-- Exercício 10: Crie uma tupla em que o primeiro elemento tem metade dos anos bissextos e o segundo elemento a outra metade.
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

split :: [a] -> ([a], [a])
split yearList = splitAt (((length yearList) + 1) `div` 2) yearList  

main :: IO()

main = do
    let years = [1..2017]
    let list = [x | x <- years, isLeap x]
    print $ split list