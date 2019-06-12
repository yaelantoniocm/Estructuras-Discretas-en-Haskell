suma:: Int -> Int -> Int
suma:: 0 m = m 
suma:: n m = suma (pred n) (succ m)

factorial ::  Int -> Int 
factorial 0=1
factorial 1=1
factorial n = n*factorial (n-1)

par:: Int->Bool 
par 0=True
par 1= False 
par n= par (n-2)

impar:: Int -> Bool
impar 0= False
impar 1= True
impar n = impar (n-1)

par :: Int -> Bool
par 0= True
par n= impar (predn)

impar::Int-> Bool
impar 0= False
impar n=Par(prend n)

car:: [a] -> a
car [e]= e
car (x:xs)= x

cdr :: [a] -> [a]
cdr [e] = []
cdr (x:xs) = xs

ultimo :: [a] -> a
ultimo [e]= e
ultimo [x:xs]= ultimo xs

toma :: Int -> [a]->[a]
toma _ [] = []
toma 0 [] = []
toma n (x:xs)= x:(toma(pred n) xs)

deja :: Int -> [a] -> [a]
deja 0 k=k
deja n [] =  []
deja n (x:xs) =deja (predn) xs

conc [a]-> [a]-> [a]-> [a]
conc [] [] = []
conc [] m = m
conc (x:xs) m = x:(conc xs m)

merge :: [a] -> [a] -> [a]
merge :: [] []= []
merge [] l= l
merge l [] =l
merge (x:xs) (y:ys)= x:y:(merge xs ys)

