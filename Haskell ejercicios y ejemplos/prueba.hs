-- |toma n>0 y regresa lałista de Naturales de s(0) hasta n
{-
 e.g
 >>> 6
 [1,2,3,4,5,6]
-}
g :: Int -> [Int]
g n = case n of 1 -> [1]; otherwise -> g(n-1) ++ [n]
             