import Data.List

potencia [] = [[]] 
potencia (x:xs) = let potencia_xs = potencia xs
              in potencia_xs++[(x:z) | z <- potencia_xs]

main = print(potencia [1,2,3])