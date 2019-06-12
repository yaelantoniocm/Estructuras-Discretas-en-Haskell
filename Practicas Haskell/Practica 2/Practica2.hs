--ESTRUCTURAS DISCRETAS 2019-1
--Práctica 2

module Practica2 where

--DEFINICIÓN DE LISTAS

--1.1: Naturales.
nat = [0..]
--1.2: Multiplos de diez.
multiplosDiez = [10,20..]
--1.3: potencias de 2.
potenciasDos = [2**n | n <- [0..]]
--1.4: Números pares.
pares = [0,2..]
--1.5: años desde el año de tu nacimiento.
anosVividos = [2000..2018]


--DEFINICIÓN DE FUNCIONES
--Saca el fibonacci, el cual suma de forma recursiva antecesor (n-1) con el antesesor del antesesor (n-2) del numero dado ('n') asi saca el fibonacci de "n"
--Ejercicio 2.1:
fibonacci :: Int -> Int
fibonacci 0 = 0 
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

--Ejercicio 2.2:
--Va a comparar si el elemto dado esta dentro de la lista, empieza comprobando si esta en la cabeza y despues se va recorriendo, por eso se tomo de forma recursiva volviendo a llamar a
--"elemento" para que vuelva a empezar pero ahora tomando el siguiente elemento y asi sucesivamente 
elemento :: (Eq a) => [a] -> a -> Bool
elemento [] n = False
elemento (x:xs) n = if (n==x) then True else elemento xs n   



--Ejercicio 2.3:
--Va a sumar todos los numeros dentro de la lista, empezando por la cabeza, y se ira recorriendo al sigueinte elemento, asi sucesivamente, por eso se llama recursivamente, para que vuelva a iniciar "sumaLIsta"
--pero ahora tomando el siguiente elemento
sumaLista ::(Num a) => [a] -> a
sumaLista [] = 0 
sumaLista (x:xs) = x + sumaLista  xs   


--Ejercicio 2.4:
--Se le va a pasar una lista de numeros y me va a decir que mes pertenece cada numero, el caso base lo puse por si la lista de numeros solo tiene un elemento (numero) solo devuelva un mes, tome las funcion "mes" de la practica 1 para este ejercicio
--y al hacerlo de forma recursiva le dije que tome el primer numero de la lista o sea la cabeza y tome el numero de la funcion mes y muestre al mes que pertenece y que despues la volvia a llamar para que tomara el elemento siguiente e  hiciera lo mismo 
meses :: [Int] -> [String]
meses [] = ["Nada"]
meses [x] = [mes x]
meses (x:xs) = mes x: meses xs -- + [mes xs] 


mes :: Int -> String
mes 1 = "Enero"
mes 2= "Febrero"
mes 3= "Marzo"
mes 4= "Abril"
mes 5="Mayo"
mes 6= "Junio"
mes 7= "Julio"
mes 8= "Agosto"
mes 9= "Septiembre"
mes 10= "Octubre"
mes 11= "Noviembre"
mes 12 = "Diciembre"


--Ejercicio 2.5:
-- Le pedi que tome una x ( los numero divisibles de ese numero) cualquiera tal que x tiene que estar desde el una hasta el numero "n" -1, o sea en antecesor, y que el modulo de n & x sea igual a 0, lo que quiere decir que si es igual a 0 
--es divisor de ese nunmero "n"
divisoresPropios :: Int -> [Int]
divisoresPropios n = [x | x <- [ 1..n-1], n `mod` x == 0]


--Ejercicio 2.6:
--Le pedi que sumara todos los divisores propios del numero "n" y si era igual al numero dado, que dijera que era verdadero.
esPerfecto :: Int -> Bool
esPerfecto n = sumaLista (divisoresPropios n) == n  


--Ejercicio 2.7:
--al igual que es perfecto pedi que sumara con "sumaLIsta" los numeros divisores "divisoresPropios" del primer numero "a" y la suma tenia que dar el segundo numero "b" y de igual forma solo que ahora la suma de los divisores de el segudno numero debia ser igual al primer numero
sonAmigos :: Int -> Int -> Bool
sonAmigos a b = sumaLista (divisoresPropios a) == b &&  sumaLista (divisoresPropios b) == a 


--Ejercicio 2.8:
-- le pedi que tomara el modulo del numero y de 10 y recursivamente lo volvi a llamar para repedir el proceso
supersuma :: Int -> Int
supersuma 0 = 0
supersuma n = n `mod` 10 + supersuma (n `div` 10)


--Ejercicio 2.9:
-- Tome todos los numeros del 0 al 9 y tome la funcion "concatenar" de la practic 1, puse un if para tomar caso de que si el numero es divisible entre 10, o sea que es su multiplo, se ponga el numero concatenandole un "ju" que es el 10 y si no de manera 
--un numero dibisible entre 10, (sin tomar en cuenta el punto decimal) de manera recursiva llamando que contatene la contatenacion de el numero divisible entre 10 con el numero que no es divisible, o sea, el 37, solo el 30 es divisible, por ende
--entonces el 7 que se contatene al 30 tomando que ya esta definido el nombre porque es divisible entre 10
japones :: Int -> String
japones 0 = "rei"
japones 1 = "ichi"
japones 2 = "ni"
japones 3 = "san"
japones 4 = "yon"
japones 5 = "go"
japones 6 = "roku"
japones 7 = "nana"
japones 8 = "haci"
japones 9 = "kyu" 
japones n = if (n `mod` 10) == 0 then concatena(japones(n `div` 10)) " ju " else concatena(concatena(japones(n `div` 10)) " ju ") (japones(n `mod` 10))



concatena :: [a] ->[a] -> [a]
concatena [] ys = ys
concatena (x:xs) ys = x : (concatena xs ys)