--
-- Copyright (c) 2017 Daniel Gonçalves da Silva - https://github.com/danielgoncalvesti
-- GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
--
module Main where

 mult3  :: Integer -> Bool
 mult3 x = (x `rem` 3) == 0

 bool2str :: Bool -> String
 bool2str True = "True"
 bool2str False = "False"

 main :: IO ()
 main = do 
         print("mult3 0: " ++ bool2str (mult3 0))
         print("mult3 1: " ++ bool2str (mult3 1))
         print("mult3 3: " ++ bool2str (mult3 3)) 
         print("mult3 4: " ++ bool2str (mult3 4))
