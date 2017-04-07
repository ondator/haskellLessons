module Monads.State where
import Data.Monoid

data Reader r a = Reader { runReader :: (r -> a) }

local' :: (r -> r1) -> Reader r1 a -> Reader r a
local' f m = Reader $ \e -> runReader m (f e)

instance Monad (Reader r) where
  return x = Reader $ \_ -> x
  m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r
  


newtype Writer w a = Writer {runWriter :: (a, w)}

writer = Writer

execWriter = snd . runWriter

instance (Monoid w) => Monad (Writer w) where
 return x = Writer (x, mempty)
 m >>= k = 
    let (x, u) = runWriter m
        (y, v) = runWriter $ k x
    in Writer (y, u `mappend` v)

tell w = writer ((),w)

evalWriter :: Writer w a -> a
evalWriter = fst . runWriter

newtype State s a = State {runState :: s -> (a, s)}

instance Monad (State s) where
  return a = State $ \st -> (a, st)
  m >>= k  = State $ \st -> 
      let (a, st') = runState m st
          m' = k a
      in runState m' st'

readerToState :: Reader r a -> State r a
readerToState m = State $ \e -> ((runReader m e), e)

writerToState :: Monoid w => Writer w a -> State w a
writerToState m = let (x, u) = runWriter m
				  in State $ \st -> (x, st `mappend` u)
