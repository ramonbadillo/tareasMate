import Data.List
import Data.Array
import Data.List.Split

v1 = array (1,3) [(3,4.5),(2,2.0), (1,1.1)]

m = array ((0,0),(3,2)) [((0,0), 1), ((0,1), 0), ((0,2),  0),
						 ((1,0), 0), ((1,1), 2), ((1,2),  0), 
						 ((2,0), 2), ((2,1), 3), ((2,2),  2),
						 ((3,0), 0), ((3,1), 3), ((3,2),  3)]


automata ::Char->Int -> Int
automata x edo | x=='/' = m!(edo, 0)
			   | x=='*' = m!(edo,1)
			   | otherwise = m!(edo,2)

funcion2 :: [Char] -> Int -> String-> String
funcion2 (x:xs) edo final|xs == [] = final
						 | edo == 2 = funcion2 xs (automata x edo) final++[x]
			   			 | otherwise  = funcion2 xs (automata x edo) final

convertir :: String -> [String]
convertir str = splitOn "" str

txt :: String
txt = "/*ramon*/ /*b*/ /*c*/ "

main = print (funcion2 (reverse txt) 0 "")