module Lists where

import Prelude hiding (foldr)
import Data.List hiding (foldr)
import Data.Char

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements a b list = a : b : list

nTimes:: a -> Int -> [a]
nTimes val count = aggregate val count [] where
						aggregate v 0 res = res
						aggregate v c res = aggregate v (c - 1) (v : res)
						
oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x : xs) | odd x  = x : oddsOnly xs
oddsOnly (x : xs) | even x = oddsOnly xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = l == reverse l

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 xs ys zs = reverse (aggregate xs ys zs []) where
					aggregate [] [] [] results = results
					aggregate [] (w:ws) (e:es) results = aggregate [] ws es ((w + e) : results)
					aggregate (w:ws) [] (e:es) results = aggregate ws [] es ((w + e) : results)
					aggregate (w:ws) (e:es) [] results = aggregate ws es [] ((w + e) : results)
					aggregate [] [] (e:es) results = aggregate [] [] es (e : results)
					aggregate (e:es) [] [] results = aggregate es [] [] (e : results)
					aggregate [] (e:es) [] results = aggregate [] es [] (e : results)					
					aggregate (q:qs) (w:ws) (e:es) results = aggregate qs ws es ((q + w + e) : results)
					
groupElems :: Eq a => [a] -> [[a]]
groupElems l  = reverse (aggregate l []) where
					aggregate [] res 		     = res
					aggregate (x:xs) [] 		 = aggregate xs [[x]]
					aggregate (x:xs) (r:results) | x == (head r) = aggregate xs ((x : r) : results)
					aggregate (x:xs) (r:results) = aggregate xs ([x] : (r : results))
				
readDigits :: String -> (String, String)
readDigits s = span isDigit s


filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj p1 p2 [] = [] 
filterDisj p1 p2 (x:xs) 
			| p1 x || p2 x = x : filterDisj p1 p2 xs
			| otherwise = filterDisj p1 p2 xs
			
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (p:xs) = (qsort lesser) ++ [p] ++ (qsort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs
		
squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes [] = []
squares'n'cubes (x:xs) = (x^2) : (x^3) : squares'n'cubes xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ init [] = init
foldr f init (x:xs) = x `f` foldr f init xs

concatList :: [[a]] -> [a]
concatList = foldr (++) []

sumAll = foldr (+) 0

lengthList :: [a] -> Int
lengthList = foldr (\x s -> 1 + s) 0

sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if odd x then x + s else s) 0

meanList :: [Double] -> Double
meanList s = (foldr (+) 0 s) / (fromIntegral (length s))

-- evenOnly :: [a] -> [a]
evenOnly s = foldr check [] (getArray s) where
				   check (y , x) ys | even (y + 1) = [x] ++ ys
							        | otherwise = [] ++ ys
				   getArray s = map (\i -> (i , s !! i)) [0..length s - 1]
				   
lastElem = foldl1 getSec where
				  getSec x y = y

