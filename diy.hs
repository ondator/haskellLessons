module DIY where
doItYourself = f . g . h where
	f = \x -> logBase 2 x
	g = \x -> x ^ 3
	h = \x -> max x 42
