-- Exercício 02: Crie uma função tipoTriangulo que determina o tipo do triângulo formado pelos três lados x, y, z.
module Main where

tipoTriangulo :: Integer -> Integer -> Integer -> String
tipoTriangulo x y z 
   | x == y && x == z = "Triângulo Equilátero"
   | x == y || y == z || z == x = "Triângulo Isosceles"
   | x /= y || y /= z || z /= x = "Triângulo Escaleno"

main :: IO()
main = do
     print(tipoTriangulo 10 10 10)
     print(tipoTriangulo 10 11 10)
     print(tipoTriangulo 9 10 11)