-- Exercício 05: Faça uma função para calcular o produto escalar entre dois vetores.
produtoEscalar :: Num a => [a] -> [a] -> a
produtoEscalar (x:xs) (y:ys) = multiplicar (x:xs) (y:ys)
    where
    multiplicar [] [] = 0
    multiplicar (x:xs) (y:ys) = x*y + multiplicar xs ys

main = do
    print (produtoEscalar [2,-4] [4,2 ])
