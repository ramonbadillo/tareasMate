import Data.List

conjuntop [] = [[]] 
conjuntop (x:xs) = let conjuntop_xs = conjuntop xs
              in conjuntop_xs++[(x:z) | z <- conjuntop_xs]

potencia ::(Int)-> Int
potencia 0 = 1
potencia n= 2 * potencia (n-1)

cardinalidad::[Int]->Bool
cardinalidad []= False
cardinalidad (x:xs) =  potencia (length (x:xs)) ==length (conjuntop (x:xs))

main = print (cardinalidad [1,2,3,4], conjuntop [1,2,3,4])