-- Exercício 02: Crie uma função projectEuler5 que retorna o primeiro número natural que retorna True para a função do exercício anterior. Pense em como reduzir o custo computacional.
divisivel20 :: Integer -> Bool
divisivel20 x = dividir x 20

dividir :: Integer -> Integer -> Bool
dividir x y
       | y == 1 = True
       | x `mod` y /= 0 = False
       | otherwise = dividir x (y-1)

main = do
     print(divisivel20(foldl lcm 1 [1..20]))
