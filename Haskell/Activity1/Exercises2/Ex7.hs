-- Exercício 07: Faça uma função que calcule o coeficiente binomial de (m,n).
binomialCoefficient :: Int -> Int -> Int
binomialCoefficient n k
  | k == 0 = 1
  | n < k  = 0
  | n > 0 && k > 0 = binomialCoefficient (n - 1) k +
                     binomialCoefficient (n - 1) (k - 1)

main = do
    print(binomialCoefficient 5 2)
