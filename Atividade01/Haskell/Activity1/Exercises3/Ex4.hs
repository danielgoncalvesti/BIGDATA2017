-- Exercício 04: Utilizando a lista anterior, calcule a soma dos números de Fibonacci pares dos valores que não excedem 4.000.000. (Project Euler 2)
fibonacci:: Int -> [Int]
fibonacci x = fib' x
  where
  fib' 0 = [0]
  fib' 1 = [1, 0]
  fib' n = (head (fib' (n-1)) + head (fib' (n-2))) : fib' (n-1)

sumSequence :: [Int] -> Int
sumSequence list = sum listComp'
        where
        listComp' = [ x | x <- list, x < 4000000, even x]

main = do
    print(sumSequence(fibonacci 200))
