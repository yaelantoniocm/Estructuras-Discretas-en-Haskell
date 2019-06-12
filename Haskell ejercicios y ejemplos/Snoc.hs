module Snoc where

 --Tipo de dato algebraico para definir listas Snoc
 data SnocList a = Mt
                 | Snoc (SnocList a) a
                 deriving (Eq, Ord, Show)

 --Ejercicio1.1
 --La función addSnoc recibe una lista Snoc, un elemento e y agrega a e como último elemento de la lista.
 addSnoc :: SnocList a -> a -> SnocList a
 addSnoc (Snoc x y) e = Snoc (Snoc x y) e    

 --Ejercicio1.2
 --La función ultimo regresa el último elemento de una lista Snoc.
 ultimo :: SnocList a -> a
 ultimo (Snoc x y) = y
 
 --Ejercicio1.3
 --La función resto regresa todos los elementos a excepción del último de una lista Snoc.
 resto :: SnocList a -> SnocList a
 resto (Snoc x a)= x

 --Ejercicio1.4
 --La función cabeza regresa el primer elemento de una lista Snoc.
 cabeza :: SnocList a -> a
 cabeza (Snoc Mt y) = y
 cabeza (Snoc x y) = cabeza x

 --Ejercicio1.5
 --La función cola regresa todos los elementos a excepción del primero de una lista Snoc.
 cola :: SnocList a -> SnocList a
 cola (Snoc Mt y) = Mt
 cola (Snoc x y) =  Snoc (cola x) y 

 --Ejercicio1.6
 --La función longitud que regresa la cantidad de elementos de una lista Snoc.
 longitud :: SnocList a -> Int
 longitud (Snoc Mt y) = 1
 longitud (Snoc x y) = 1 + longitud x 
