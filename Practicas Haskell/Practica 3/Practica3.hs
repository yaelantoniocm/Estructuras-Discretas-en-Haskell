module Practica3 where 

 import Binario

--Ejercicio 2.1
--Función que recibe una lista de enteros y devuelve una lista con
--su representación en binario
 binarios :: [Int] -> [Binario]
 binarios l = map natToBin l

--Ejercicio 2.2
--Función que recibe una lista de binarios y devuelve una lista
--con los binarios pares.
 pares :: [Binario] -> [Binario]
 pares [] = []
 pares ( (Uno (x)):xs ) = pares xs
 pares ( (Cero (x)):xs ) = (Cero (x)):(pares xs)

--Ejercicio 2.3
--Función que recibe una lista de Strings y devuelve una lista con las cadenas
--de longitud menor o igual a 7.
 tooLong :: [String] -> [String]
 tooLong [] = []
 tooLong (x:xs) 
  | tooLong2 x 7 = x:(tooLong xs )
  | otherwise = tooLong xs
--Función complementaria de tooLong. Recibe una cadena y un número
--Regresa verdadero si y solo si la cadena tiene un tamaño igual o menor a 7
 tooLong2 :: String -> Int -> Bool
 tooLong2 (x:xs) n = tooLong2 xs (n-1)
 tooLong2 [] n
  | 7 >= n && n >= 0 = True
  | otherwise = False

--Ejercicio 2.4
--Función que recibe un entero y devuelve una lista con la serie de fibonacci
--hasta la posición n+1.
 sFibonacci :: Int -> [Int]
 sFibonacci n = map fibonacci [1..n+1]

--Función que recibe un entero y devuelve el número de fibonacci
--en esa posición.
 fibonacci :: Int -> Int
 fibonacci 1 = 0
 fibonacci 2 = 1
 fibonacci n = fibonacci (n-1) + fibonacci (n-2)

--Ejercicio 2.5
--Función que recibe una lista y un objeto de mismo tipo
--y devuelve una lista sin el elemento.
 quitaElemento :: (Eq a) => [a] -> a -> [a]
 quitaElemento l a = filter (\x-> x/=a) l