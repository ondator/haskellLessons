module Monads.Writer where

import Data.Monoid

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


type Shopping = Writer (Sum Integer, [String]) ()

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328
  
purchase :: String -> Integer -> Shopping
purchase item cost = tell $ (Sum cost, [item]) 

total :: Shopping -> Integer
total = getSum . fst . execWriter

items :: Shopping -> [String]
items = snd . execWriter