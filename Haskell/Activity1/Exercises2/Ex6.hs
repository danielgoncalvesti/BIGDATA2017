-- Exercício 06: Faça uma função que calcule a persistência aditiva de um número.
additivePersistence :: Int -> Int
additivePersistence x 
            |x < 10 = 0
            |otherwise = (additivePersistence (sum $ numSum x)) + 1
            where 
              numSum 0 = [0]
              numSum x = numSum (x `div` 10) ++ [x `rem` 10]

main = do
    print (additivePersistence 111)
