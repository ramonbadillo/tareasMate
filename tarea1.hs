#!/usr/bin/env runhaskell

xor :: Bool -> Bool -> Bool
xor x y | x == True && y == False = True
        | x == False && y == True = True
        | otherwise = False

equiv3 ::  (Bool ->Bool -> Bool ->Bool) -> (Bool->Bool->Bool->Bool) -> Bool
equiv3 func1 func2 = and [ 
		(func1 a b c)==(func2 a b c )  |  							
		a <- [True,False], b <-[True,False], c <-[True,False]
	]
	
	
equiv2 ::  (Bool ->Bool -> Bool) -> (Bool->Bool->Bool) -> Bool
equiv2 func1 func2 = and [ 
		(func1 a b )==(func2 a b )  |  							
		a <- [True,False], b <-[True,False]
	]

main = print (equiv3 
	 	(\ p q r -> (p <= q ) <= r) --15
	 	(\ a b c-> a <= ( b <= c)),	
	 	equiv2
 		(\ p q -> ( p <= q )) 	-- 16
 		(\ a b -> ( not b ) <= (not a)),
 		equiv2
 		(\ p q -> (not p) ==  q ) 	-- 17
 		(\ a b -> a ==(not b)),
 		equiv2
 		(\ p q -> not (xor p q)) 	-- 18
 		(\ a b -> a ==b), 	
		equiv2
 		(\ p q  -> not q <= not p)--19
		(\ a b ->  a <= b),
		equiv3 --20
		(\ p q r -> (p <= q ) && (p <= r ))
		(\ a b c ->  a <= (b && c)),
		equiv3 --21
		(\ p q r -> (p <= r ) && (q <= r ))
		(\ a b c -> (a || b) <= c),
		equiv3 --22
		(\ p q r -> (p <= q ) || (p <= r ))
		(\ a b c -> a <= (b || c)),
		equiv3 --23
		(\ p q r -> (p <= r ) || (q <= r ))
		(\ a b c -> (a && b) <= c),
		equiv3 --24
		(\ p q r -> not p <= ( q <= r ) )
		(\ a b c -> b <= ( a || c ) ),
		equiv2 --25
		(\ p q  -> p == q)
		(\ a b -> (a <= b) && (b <= a)),
		equiv2 --26
		(\ p q -> p == q )
		(\ a b -> (not a) == (not b))
		
		)
	
	
--main = print(valid15A test15A == valid15B test15B)



--PROBLEMA 17-------------------
valid17A :: (Bool -> Bool -> Bool) -> Bool
valid17A func = and [func a b | a <- [True,False] , b <- [True,False]  ]


valid17B :: (Bool -> Bool -> Bool) -> Bool
valid17B func = and [func a b | a <- [True,False] , b <- [True,False]  ]
--test17B p q = p => not q






--main = print(valid17B test17B == valid17A test17A)