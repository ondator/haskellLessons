module Monads.Monad where

data Log a = Log [String] a deriving Show

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = Log [msg] . f

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = case f x of Log [ms] r 
                                 -> case g r of Log [ls] t -> Log (ms:[ls]) t
								 
returnLog :: a -> Log a
returnLog a = Log [] a


bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log startLog ini) f = Log (startLog++newLog) result where 
                                           Log newLog result = f ini
										   
instance Monad Log where
    return = returnLog
    (>>=) = bindLog
	
execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList x fs = exec (return x) fs where
                              exec l [] = l
                              exec l (g:gs) = exec (l >>= g) gs
							  
							  
data SomeType a = SomeType a deriving Show

instance Monad SomeType where
     return = SomeType
     (>>=) (SomeType a) f = f a
							  
instance Functor SomeType where
    fmap f x = x >>= (return . f)