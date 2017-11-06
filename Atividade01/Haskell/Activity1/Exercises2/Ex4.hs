-- Exercício 04: Faça uma função que determine se um número é primo.
module Main where

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n 
       | (length [x | x <- [2 .. n-1], mod n x == 0]) > 0 = False
       | otherwise = True

main :: IO ()
main = do
  print (isPrime 2)
  print (isPrime 3) 
  print (isPrime 4)
  print (isPrime 5) 
  print (isPrime 6)
  print (isPrime 997)