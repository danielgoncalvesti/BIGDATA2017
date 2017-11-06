-- Exercício 03: Faça uma função que calcule a soma da diagonal secundária de uma matriz.
sumDiagonalReverse :: (Num a) => [[a]] -> a
sumDiagonalReverse d = sum (sumDiagonal (reverse d))
      where
      sumDiagonal [[]]  =  [0]
      sumDiagonal (xs:[]) = [head xs] 
      sumDiagonal (x:xs) = head x : sumDiagonal (map tail xs)
      

identityMatrix :: Int -> [[Int]]
identityMatrix n = [[fromEnum $ x == y | x <- [1..n]]| y <- [1..n]]      

main = do
    print (identityMatrix 15)
    print (sumDiagonalReverse $ identityMatrix 15)
