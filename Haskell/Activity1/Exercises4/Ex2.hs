-- Exercício 02: Faça uma função que calcule a soma da diagonal principal de uma matriz.
sumDiagonal :: (Num a) => [[a]] -> a
sumDiagonal d = sum (sumDiagonal d)
      where
      sumDiagonal [[]]       = []
      sumDiagonal (xs:[])    = [head xs]
      sumDiagonal (x:xs)     = head x : sumDiagonal (map tail xs)

identityMatrix :: Int -> [[Int]]
identityMatrix n = [[fromEnum $ (x == y) | x <- [1..n]]| y <- [1..n]]      

main = do
    print $ sumDiagonal $ identityMatrix 12
