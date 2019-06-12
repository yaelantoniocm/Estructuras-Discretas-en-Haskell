--Calzada Martín Yael Antonio 316270346
--Mauricio Mercado Aguero     316284567
--Dylan Emmanuel Radilla      316237848
data AE = Var String | Const Int | Op Opera AE AE 
data Opera = Suma | Resta | Mul | Div  


eval :: AE -> Int
eval (Const x) = x 
eval (Var x) = error "Error"
eval (Op Suma x y) = eval x + eval y 
eval (Op Resta x y) = eval x - eval y 
eval (Op Mul x y) = eval x * eval y 
eval (Op Div x y) = eval x `div` eval y 


posNeg :: [Int] -> [String]
posNeg l = map (\x -> (if x < 0 then "negativo" else "positivo")) l
