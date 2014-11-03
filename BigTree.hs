data BinTree = L | N BinTree BinTree deriving Show

makeBinTree :: Integer -> BinTree
makeBinTree 0 = L
makeBinTree (n) = N (makeBinTree (n-1)) (makeBinTree (n-1))

count :: BinTree -> Integer
count L = 1
count (N t1 t2) = 1 + count t1 + count t2

main = print(makeBinTree 6, count(makeBinTree 6)) 