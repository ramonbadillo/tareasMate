import Control.Monad

data NFA q s = NFA
    { intialState :: q
    , isAccepting :: q -> Bool
    , transition  :: q -> s -> [q]
    }

testNFA :: NFA q s -> [s] -> Bool
testNFA (NFA i a t) = any a . foldM t i

data State  = P | Q | R  deriving (Eq, Show)
data Symbol = A | B | C  deriving (Eq, Show)

-- initial state
i = P

-- accept criteria
a = (`elem` [R])

-- state transitions
t P A = [P]
t P B = [P, Q]
t P C = [P, Q, R]
t Q A = [P, Q]
t Q B = [P, Q, R]
t Q C = [P, Q, R]
t R A = [P, Q, R]
t R B = [P, Q, R]
t R C = [P, Q, R]
-- t _  _ = []

nfa = NFA i a t

main = do{
	print(testNFA nfa [A,B,B]);
	print(testNFA nfa [A,B,C]);
	print(testNFA nfa [A,A,A]);
	print(testNFA nfa [B,B,B]);
	print(testNFA nfa [B,B,A]);
	print(testNFA nfa [C]);
	print(testNFA nfa [C,C,C]);
	print(testNFA nfa [C,C,A]);
}
