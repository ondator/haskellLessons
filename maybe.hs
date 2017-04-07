module Monads.Maybe where

import Data.Char


data Token = Number Int | Plus | Minus | LeftBrace | RightBrace     
     deriving (Eq, Show)
-- Тип Token уже объявлен, его писать не нужно

asToken :: String -> Maybe Token
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace
asToken s | all isDigit s = (Just . Number . read) s
asToken s | otherwise = Nothing

tokenize :: String -> Maybe [Token]
tokenize = sequence . (map asToken) . words


data Board = A | B | C deriving (Eq, Show)
nextPositions :: Board -> [Board]
nextPositions b = [b,b,b,A,B,C]


nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b n pred = filter pred (allNextPositionsN n b) where
               allNextPositionsN n b | n < 0 = []
                                     | n == 0 = [b]
                                     | otherwise = do 
                                               r <- nextPositions b
                                               return r >>= (allNextPositionsN (n - 1))

											   
pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x | x <= 0 = []
                    | otherwise = do

  a <- [1..x]
  b <- [1..x]
  c <- [1..x]
  
  True <- return $ (a < b) && (a ^ 2 + b ^ 2 == c ^ 2)
  return (a , b , c)