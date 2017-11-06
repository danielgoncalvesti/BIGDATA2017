-- Exercício 09b: Encontre os 10 últimos anos bissextos (dica: use a função length para determinar o tamanho da lista).
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
  
inverse :: [a] -> [a]
inverse [] = []
inverse (x:xs) = (inverse xs) ++ [x]
  
main :: IO()

main = do
    let yearsReverse = inverse [1..2017]
    let lista =  [x | x <- yearsReverse, isLeap x]
    print(lista)