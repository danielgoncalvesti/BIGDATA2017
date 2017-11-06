--
-- Copyright (c) 2017 Daniel Gonçalves da Silva - https://github.com/danielgoncalvesti
-- GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
-- 
module Main where

middleSin :: Double -> (Double, Double)
middleSin x = (sqrt ((1- cos x)/2),- sqrt ((1- cos x)/2))

main :: IO()

main = do
    print(middleSin 90)