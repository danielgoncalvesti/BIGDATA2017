-- Exercício 01: Faça uma função que gere uma matriz identidade de tamanho n.
identityMatrix :: Int -> [[Int]]
identityMatrix n = [[fromEnum $ (x == y) | x <- [1..n]]| y <- [1..n]]

main = do
    print(identityMatrix 5)
