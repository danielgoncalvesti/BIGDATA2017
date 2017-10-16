-- Exercício 06: Faça uma função que calcule a persistência aditiva de um número.
import Data.Tuple (swap)
import Data.List (unfoldr)
 
digSum :: Int -> Int -> Int
digSum base = sum . unfoldr f
  where
    f 0 = Nothing
    f n = Just (swap (quotRem n base))
 
digRoot :: Int -> Int -> (Int, Int)
digRoot base =
  head . dropWhile ((>= base) . snd) . zip [0 ..] . iterate (digSum base)
 
main :: IO ()
main = do
  putStrLn "in base 10:"
  mapM_ (print . ((,) <*> digRoot 10)) [22, 99, 111, 4556]
