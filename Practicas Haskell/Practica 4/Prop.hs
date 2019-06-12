module Prop where 
 
 import Data.List

--DEFINICIONES 

-- Tipo de dato para representar las expresiones de la lógica proposicional
 data Prop = Verdadero
           | Falso 
           | Var String
           | Neg Prop
           | Conj Prop Prop
           | Disy Prop Prop
           | Impl Prop Prop
           | Syss Prop Prop
           deriving (Eq,Ord)

-- Sinónimo para representar el estado
 type Estado = [(String, Prop)]

--Instancia de Show para el tipo Prop, para que sea legible lo que se imprime en consola
-- NO DEFINE COMPORTAMIENTO
 instance Show Prop where 
  show Verdadero = "V" -- V
  show Falso = "F" -- F
  show (Var x) = x -- P 
  show (Neg p) = "¬ " ++ show p -- ¬ P
  show (Conj p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")" -- (P ∧ Q)
  show (Disy p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")" -- (P ∨ Q)
  show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")" -- (P → Q)
  show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")" -- (P ↔ Q)

-- EQUIVALENCIAS LÓGICAS 

 -- Ejercicio 1.1
 eliminacion :: Prop -> Prop
 eliminacion (Var p) = Var p
 eliminacion (Neg p) = Neg (eliminacion p)
 eliminacion Verdadero = Verdadero
 eliminacion Falso  = Falso
 eliminacion (Impl p q) = Disy (Neg (eliminacion p)) (eliminacion q)
 eliminacion (Syss p q) = (Conj (Disy (Neg (eliminacion p)) (eliminacion q)) (Disy (Neg (eliminacion q)) (eliminacion p)))
 eliminacion (Conj p q) = (Conj (eliminacion p) (eliminacion q))
 eliminacion (Disy p q) = Disy (eliminacion p) (eliminacion q)

 -- Ejercicio 1.2
 deMorgan :: Prop -> Prop
 deMorgan (Var x) = Var x
 deMorgan (Conj x y) = Conj (deMorgan x) (deMorgan y)
 deMorgan (Disy x y) = Disy (deMorgan x) (deMorgan y)
 deMorgan (Neg (Conj x y)) = Disy ( Neg (deMorgan x)) (Neg (deMorgan y))
 deMorgan (Neg (Disy x y)) = Conj ( Neg (deMorgan x)) (Neg (deMorgan y))
 deMorgan (Syss x y) = Syss (deMorgan x) (deMorgan y)
 deMorgan (Impl x y) = Impl (deMorgan x) (deMorgan y) 

-- EVALUACIÓN Y ANÁLISIS SINTÁCTICO DE EXPRESIONES

 
 variables :: Prop -> [String]
 variables (Var x) = [x]
 variables (Neg p) = variables p
 variables (Conj p q) =  variables p `union` variables q
 variables (Disy p q) =  variables p `union` variables q
 variables (Impl p q) =  variables p `union` variables q
 variables (Syss p q) =  variables p `union` variables q
 variables p = []

 
 quitaRepetidosPar ::(Eq a) => [(a,b)] -> [(a,b)]
 quitaRepetidosPar [] = []
 quitaRepetidosPar (x:xs) = x:(quitaRepetidosPar $ filter (\y -> fst x /= fst y) xs)

 
 estados :: Prop -> [Estado]
 estados p = nub 
            $ map sort
              
              $ map quitaRepetidosPar
             
              $ permutations [(x,y) | x <- (variables p), y <- [Verdadero,Falso]]

-- Ejercicio 2.1 

 busca :: String -> Estado -> Bool
 busca s e = case (head (map (\x -> snd(x))(filter (\x -> (s == fst (x))) e))) of Verdadero -> True; otherwise -> False

 interp :: Prop -> Estado -> Bool
 interp (Var x) y = busca x y
 interp (Verdadero) _ = True
 interp (Falso) _ = False
 interp (Neg x) y = not (interp x y)
 interp (Disy p q) y = interp p y || interp q y
 interp (Conj p q) y = interp p y && interp q y
 interp (Impl p q) y = interp (eliminacion (Impl (p) (q))) y
 interp (Syss p q) y = interp (eliminacion (Syss (p) (q))) y
  
 --  -- Ejercicio 2.2

 truthTable :: Prop -> String
 truthTable x
  | (b == 0) = "Contradiccion"  
  | (length a == b) = "Tautologia"
  | otherwise = "Contingencia"
            where
              a = map (interp x) (estados x)
              b = length (filter (\ x -> (if (x == True) then True else False)) a)

 --unir :: [Prop] -> Prop 
 --unir [] = Verdadero
 --unir x:xs= unir (Conj (x)(xs))



 -- -- Ejercicio 2.3  
 
 --correcto :: [Prop] -> Prop -> Bool 
 --correcto (Impl (Conj p q)(Var x) = map truthTable p q)
