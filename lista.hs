import Data.List

splitList (h:xs) = splitListAux [h] xs
splitListAux l (h:xs) | xs==[] = [(l, [h])]
	| otherwise = (l, h:xs): splitListAux x xs
	where x = l++[h]
--splitList[x]= [x:xs] [([x], [x+1:xs])]
--splitList [x] 

mylast :: [a]->a
mylast [x]=x
mylast (_:xs)=mylast xs

--potencia :: [Int]->[Int]

subsets [] = [[]] 
subsets (x:xs) = let subsets_xs = subsets xs
              in subsets_xs++[(x:z) | z <- subsets_xs]

main = print(subsets [1,2,3])
