a =[1,2,3]
a =[3,4,5]

c = [ x| x <- a, (elem x b)]

u = [ (x,y) | x <- a, x <-b]
