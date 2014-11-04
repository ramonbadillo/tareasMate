import Data.List.Split


convertir :: String -> [String]
convertir str = splitOn "" str


prueba n (x:xs) | x == "/" = prueba 1 xs 
				| n == 1 && x == "*" = prueba 2 xs
				-- | n == 2 && x == "*" = prueba 3 xs
				| n == 2 = guarda x
				| inicio /= 0  = prueba 3 xs
				| n == 3 && x == "/"
				| otherwise = prueba 0 xs 
				
			 -- | x == "/*" = prueba xs
guarda (x)
	palabra ++ x
			  
txt :: String
txt = "*/***** and both feature characters who will do whatever it takes to " ++
  "get to their goal and in the end the thing they want the most ends " ++
  "up destroying them In case of this */ is a whale"

main = do print(prueba 0 (convertir txt))