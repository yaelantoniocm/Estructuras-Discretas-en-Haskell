module Binario where 

 --Ejercicio 1.1
 --Tipo de dato que representa números mayores o igual a 1 en binario.
 data Binario = BaseUno | Uno Binario | Cero Binario deriving (Eq)

 instance Show Binario where
  show BaseUno = "1"
  show (Cero b) = show b ++ "0"
  show (Uno b) = show b ++ "1"

--Ejercicio 1.2
--Función que recibe un entero y regresa su representación binaria.
 natToBin :: Int -> Binario
 natToBin 1 = BaseUno
 natToBin x
  | mod x 2 == 1 = Uno ( natToBin (div x 2))
  | mod x 2 == 0 = Cero ( natToBin (div x 2))

--Ejercicio 1.3
--Función que recibe un dato de tipo binario y regresa su representación entera.
 binToNat :: Binario -> Int
 binToNat BaseUno = 1
 binToNat (Uno (x)) = (2 * binToNat x) + 1
 binToNat (Cero (x)) = 2 * binToNat x

--Ejercicio 1.4
--Función que recibe un dato de tipo binario y devuelve su sucesor.
 sucesor :: Binario -> Binario
 sucesor BaseUno = Cero BaseUno
 sucesor (Cero (x)) = Uno (x)
 sucesor (Uno (x)) = Cero (sucesor (x))

--Ejercicio 1.5
--Funión que recibe un dato de tipo binario y devuelve un int que representa
--la cantidad de bits prendidos.
 bitsEncendidos :: Binario -> Int
 bitsEncendidos BaseUno = 1
 bitsEncendidos (Uno (x)) = 1 + bitsEncendidos x
 bitsEncendidos (Cero (x)) = bitsEncendidos x