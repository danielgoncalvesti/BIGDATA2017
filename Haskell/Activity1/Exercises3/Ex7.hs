-- Exercício 07: Implemente uma função collatzLen x que retorna o tamanho da lista formada pela aplicação repetida de collatz sobre o valor x até que essa chegue no número 1.
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
    
main = do
    print (collatzLen 10)
