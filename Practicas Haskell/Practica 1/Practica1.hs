--ESTRUCTURAS DISCRETAS 2019-1
--Práctica 1

--Ejercicio 1.1:
areaCirculo :: Float -> Float

areaCirculo r = pi*(r ** 2)

--Ejercicio 1.2:
distancia :: (Float , Float ) -> (Float , Float ) -> Float

distancia (x , y) (a, b) = sqrt ((a-x) ** 2 + (b-y) ** 2) 

--Ejercicio 1.3:
imp :: Bool -> Bool -> Bool

imp True False = False 
imp x y = True 

--Ejercicio 1.4:
xor :: Bool -> Bool -> Bool

xor True True = False 
xor False False = False
xor x y =True  


--Ejercicio 1.5:
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

--Ejercicio 1.6:
calculadora :: String -> (Int ,Int) -> Int
calculadora "first" (x , y) = x 
calculadora "last" (x , y)= y  
calculadora "sum" (x, y) =x + y
calculadora "rest" (x, y) = x-y 
calculadora "mul" (x, y) = x * y
calculadora "div" (x ,y) =  div x y
calculadora "pow" (x, y)= x ^ y

--Ejercicio 1.7:
loki :: Int -> Bool -> String
loki t True = if t > 20 && t<30 then "Sale a jugar" else "No sale a jugar"
loki t False = if t > 15 && t<25 then "Sale a jugar" else "No sale a jugar"

--Ejercicio 1.8:
monos :: Bool -> Bool -> String
monos True True = "Hay problemas"
monos False False = "Hay problemas"
monos True False = "No hay problemas"
monos False True = "No hay problemas"

--Ejercicio 1.9:
multiplica :: Int -> Int -> Int
multiplica 0 y = 0
multiplica 1 y = y
multiplica x y = suma y (multiplica (pred x) y)   

suma :: Int -> Int -> Int 
suma 0 n = n
suma n m = suma (pred n) (succ m)

potencia :: Int -> Int -> Int
potencia x 0 = 1
potencia x y = multiplica x (potencia x (pred y))