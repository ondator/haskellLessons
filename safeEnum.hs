module StrangeEnum where

class (Enum a, Bounded a, Eq a) => SafeEnum a where
	ssucc :: a -> a
	spred :: a -> a
	
	ssucc x | x == maxBound = minBound
			| otherwise = succ x
			
	spred x | x == minBound = maxBound
			| otherwise = pred x