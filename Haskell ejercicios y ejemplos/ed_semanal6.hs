
data AE = Var String | Const Int | Op Operacion AE AE deriving (Show)

data Operacion = Suma | Resta | Mult | Div deriving (Show)

type Estado = [(String,AE)]

opera :: Operacion -> (Int -> Int -> Int)
opera Suma = \x -> \y -> x + y 
opera Resta = \x -> \y -> x - y
opera Mult = \x -> \y -> x * y
opera Div = \x -> \y -> x `div` y

busca :: String -> Estado -> AE
busca x [] = error "La lista es vacia"
busca z (x:xs) 
 | z == fst (x) = snd (x) 
 |otherwise = busca z (xs)

eval :: AE -> Estado -> Int
eval (Const n) y = n 
eval (Var z) x = eval (busca z x) x 
eval (Op Suma z y) x = eval z x + eval y x
eval (Op Resta z y) x = eval z x - eval y x 
eval (Op Mult z y) x = eval z x * eval y x
eval (Op Div z y) x = eval z x `div` eval y x
