-- Exercício 08: Encontre o número x entre 1 e 1.000.000 que tem a maior sequência de Collatz. (Project Euler 14)
collatz :: Int -> Int
collatz x 
        |x `mod` 2 == 0 = x `div` 2
        |otherwise = (3 * x)+1

collatzSeq:: Int -> [Int]
collatzSeq 1 = [1]
collatzSeq x = x:(collatzSeq next)
    where next = collatz x

collatzLen:: Int -> Int
collatzLen x = length (collatzSeq x)

projectEuler14 :: Int
projectEuler14 = maximum [collatzLen x | x <- [1..1000000]]

main = do
    print(projectEuler14) 
