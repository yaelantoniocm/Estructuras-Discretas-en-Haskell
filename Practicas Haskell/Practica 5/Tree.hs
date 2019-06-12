module Tree where

 -- Tipo de dato Algebraico para definir Árboles Binarios
 data BinaryTree a = Void | Node (BinaryTree a) a (BinaryTree a) deriving (Eq,Ord,Show)

 --Ejercicio 2.1 
 --La función addTree agrega un elemento a un árbol binario ordenado, preservando el orden.
 addTree :: (Ord a) => BinaryTree a -> a -> BinaryTree a
 addTree Void s = Node Void s Void
 addTree (Node (n) x (m)) s
  |(s < x) = Node (addTree n s) x (m)
  |(s >= x) = Node (n) x (addTree m s) 

 --Ejercicio 2.2 
 --La función inorder recibe un BinaryTree y regresa una lista con los elementos del árbol recorriéndolo inorder.
 inorder :: BinaryTree a -> [a]
 inorder Void = []
 inorder (Node x y z) = inorder x ++ [y] ++ inorder z

 --Ejercicio 2.3 
 --la función preorder que recibe un BinaryTree y regresa una lista con los elementos del árbol recorriéndolo preorder.
 preorder :: BinaryTree a -> [a]
 preorder Void = []
 preorder (Node x y z) = [y] ++ preorder x ++ preorder z

 --Ejercicio 2.4 
 --La función postorder recibe un BinaryTree y regresa una lista con los elementos del árbol recorriéndolo postorder.
 postorder :: BinaryTree a -> [a]
 postorder Void = []
 postorder (Node x y z) = postorder x ++ postorder z ++ [y]

 --Ejercicio 2.5 
 --La función maximo regresa el elemento más grande de un BinaryTree ordenado.
 maximo :: (Ord a) => BinaryTree a -> a
 maximo (Node Void x Void) = x 
 maximo (Node x y Void) = y 
 maximo (Node x y w) = maximo w 

 --Ejercicio 2.6 
 --La función minimo regresa el elemento más pequeño de un BinaryTree ordenado.
 minimo :: (Ord a) => BinaryTree a -> a
 minimo (Node Void x Void) = x
 minimo (Node Void x y) = x
 minimo (Node x y w) = minimo x 

 --Ejercicio 2.7 
 --La función busca recibe un elemento y un BinaryTree ordenado y nos dice si el elemento pertenece o no al BinaryTree utilizando el algoritmo de búsqueda visto en clase.
 busca :: (Ord a) => a -> BinaryTree a -> Bool
 busca n (Void) = False 
 busca n (Node x y z) 
  |(n == y) = True
  |(n < y) = busca n x
  |(n >= y) = busca n z  



 