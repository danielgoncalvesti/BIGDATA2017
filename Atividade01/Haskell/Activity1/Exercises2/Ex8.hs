-- Exercício 08: Faça uma função que calcule o elemento (i,j) do triângulo de pascal.
import Data.List
pascal n = map liner [0..n]
    where liner x = map (comb x) [0..x]

comb n 0 = 1
comb 0 r = 0
comb n r = comb (n-1) (r-1) * n `div` r 

main :: IO()
main = do
    print(pascal 10)
