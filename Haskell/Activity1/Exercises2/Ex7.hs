-- Exercício 07: Faça uma função que calcule o coeficiente binomial de (m,n).
binom :: Int -> Int -> Int
binom = loop 1 1
  where
    loop rn rd _ 0 = rn `div` rd
    loop _  _  0 _ = 0
    loop rn rd n k = loop (rn * n) (rd * k) (n-1) (k-1)
    
main = do 
    print(binom 10 6)
