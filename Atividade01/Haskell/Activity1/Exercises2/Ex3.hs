-- Exercício 03: Implemente uma função que faz a multiplicação etíope entre dois números.
module Main where

multEtiope :: Integer -> Integer -> Integer
multEtiope m n = multEtiope' m n 0
   where
       multEtiope' 1 n r = n + r
       multEtiope' m n r
             | ehImpar' m = multEtiope' (div m 2) (n*2) (n+r)
             | otherwise =  multEtiope' (div m 2) (n*2) r
                where
                    ehImpar' m 
                        | m `mod` 2 == 0  = True
                        | otherwise       = False

main :: IO()
main = do
    print(multEtiope 369 120)
   