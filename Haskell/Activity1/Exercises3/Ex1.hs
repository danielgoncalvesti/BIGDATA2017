-- Exercício 01: Crie uma função divisivel20 x que retorna verdadeiro se x for divisível por todos os números de 1 a 20.
divisivel20 :: Integer -> Bool
divisivel20 x = dividir x 20

dividir :: Integer -> Integer -> Bool
dividir x y
       | y == 1 = True
       | x `mod` y /= 0 = False
       | otherwise = dividir x (y-1)
       
main :: IO()
main = do
     print(divisivel20 0) --True
     print(divisivel20 20) -- False
