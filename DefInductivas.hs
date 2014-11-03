data Nat = Z | S Nat
	deriving (Eq, Show)

suma:: Nat->Nat->Nat
suma Z n =n
suma (S n)m = S(suma n m)

multi:: Nat->Nat->Nat
multi m Z = Z
multi m (S n) =suma  (m `multi` n) m

expn::Nat->Nat->Nat
expn m Z = (S Z)
expn m (S n) = (expn m n) `multi` m

main = print(suma (S Z)  (S Z), multi (S Z)  (S Z), expn  (S (S (S Z))) (S Z))