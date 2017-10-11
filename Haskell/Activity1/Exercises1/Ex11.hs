-- Exercício 11: Crie um concatenador de strings que concatena duas strings separadas por espaço.
-- Copyright (c) 2017 Daniel Gonçalves da Silva - https://github.com/danielgoncalvesti
-- GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
--
module Main where

concatStrings :: String -> String -> String
concatStrings s1 s2 =  s1 ++ " " ++ s2 

main :: IO()
main = do 
    let string1 = "value1"
    let string2 = "value2"
    print(concatStrings string1 string2)