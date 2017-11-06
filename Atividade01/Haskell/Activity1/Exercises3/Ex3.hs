-- Exercício 03: Crie a lista de números de Fibonacci utilizando uma função geradora.
fibonacci::Int->[Int]
fibonacci x = inverteLista (fib' x)
  where
  fib' 0 = [0]
  fib' 1 = [1, 0]
  fib' n = (head (fib' (n-1)) + head (fib' (n-2))) : fib' (n-1)

inverteLista :: [a] -> [a]
inverteLista [] = []
inverteLista (x:xs) = (inverteLista xs) ++ [x]

main = do
    print(fibonacci 10)
