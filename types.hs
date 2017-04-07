module Types where

import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Data.Char(isDigit)
import Data.List.Split
import Data.Monoid
import Prelude hiding (lookup)
import qualified Data.List as L

data Color = Red | Green | Blue

instance Show Color where
	show Red   = "Red"
	show Green = "Green"
	show Blue  = "Blue"
	
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9

stringToColor :: String -> Color
stringToColor "Red" = Red
stringToColor "Green" = Green
stringToColor "Blue" = Blue


cmp :: LogLevel -> LogLevel -> Ordering

cmp Error Error = EQ
cmp Warning Warning = EQ
cmp Info Info = EQ
cmp Error _ = GT
cmp Info _ = LT
cmp _ Error = LT
cmp _ Info = GT

data Result = Fail | Success
data SomeData = T | F

doSomeWork :: SomeData -> (Result,Int)
doSomeWork T = (Success, 0)
doSomeWork F = (Fail, 155)

processData :: SomeData -> String

processData x = case doSomeWork x of
		(Success, _) -> "Success"
		(Fail, c) -> "Fail: " ++ (show c)
		
		
		
data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

-- distance :: Point -> Point -> Double
-- distance (Point x y) (Point a b)= sqrt ((x - a) ^ 2 + (y - b) ^ 2)


data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r) = pi * (r ^ 2)
area (Rectangle a b) = a * b


data Result' = Result' Result Int | Success'

doSomeWork' :: SomeData -> Result'
doSomeWork' x = case doSomeWork x of
				(Success,_) -> Success'
				(Fail, c) -> Result' Fail c

				
instance Show Result' where
	show (Result' Fail c) = "Fail: " ++ show c
	show Success' = "Success"
	
isSquare (Rectangle a b)| a == b = True
						| otherwise = False
isSquare  _ = False




timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving Show

data LogEntry = LogEntry {timestamp :: UTCTime, logLevel :: LogLevel, message :: String}

logLevelToString :: LogLevel -> String
logLevelToString = show

logEntryToString :: LogEntry -> String
logEntryToString x = (timeToString $ timestamp x) ++ ": " ++ (logLevelToString $ logLevel x) ++ ": " ++ message x 


data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

updateLastName :: Person -> Person -> Person
updateLastName person1 person2 = person2 {lastName = lastName person1}


isRectangle :: Shape -> Bool
isRectangle Rectangle{} = True
isRectangle _ = False

abbrFirstName :: Person -> Person
abbrFirstName p = p {firstName = getNewName $ firstName p} where									
									getNewName [n] = [n]
									getNewName [] = []
									getNewName (n:ns) = n : ['.']
									
									

data Coord a = Coord a a deriving Show

distance :: Coord Double -> Coord Double -> Double
distance (Coord a b) (Coord x y) = sqrt $ (x - a ) ^ 2 + (y - b) ^ 2

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord a b) (Coord x y) = abs (x - a) + abs (y - b)


getCenter :: Double -> Coord Int -> Coord Double
getCenter size (Coord i j) = Coord (size * (realToFrac i + 0.5)) (size * (realToFrac j + 0.5))

getCell :: Double -> Coord Double -> Coord Int
getCell size (Coord x y) = Coord (floor $ x / size) (floor $ y / size)




findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x:xs) | isDigit x = Just x
				 | otherwise = findDigit xs
				 
findDigitOrX :: [Char] -> Char
findDigitOrX x = case findDigit x of
					Just c -> c
					_ -> 'X'
					
					
maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing = []

listToMaybe :: [a] -> Maybe a
listToMaybe (x:xs) = Just x
listToMaybe [] = Nothing


eitherToMaybe :: Either a b -> Maybe a
eitherToMaybe (Left a) = Just a
eitherToMaybe (Right _) = Nothing

data List a = Nil | Cons a (List a) deriving Show

-- fromList :: List a -> [a]
-- fromList (Cons a ls)= a : fromList ls
-- fromList Nil = []

toList :: [a] -> List a
toList (x : xs) = Cons x $ toList xs
toList [] = Nil


data Nat = Zero | Suc Nat deriving Show

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Zero
toNat i = Suc $ toNat (i - 1)

add :: Nat -> Nat -> Nat
add a b = toNat (fromNat a + fromNat b)

mul :: Nat -> Nat -> Nat
mul a b = toNat (fromNat a * fromNat b)

fac :: Nat -> Nat
fac a = toNat $ product [1..(fromNat a)]



data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf l) = 0
height (Node l r) = 1 + max (height l) (height r)

size :: Tree a -> Int
size (Leaf l) = 1
size (Node l r) = 1 + size l + size r


avg :: Tree Int -> Int
avg t = 
    let (c,s) = go t
    in s `div` c where 
    go (Leaf l) = (1,l) 
    go (Node l r) = 
        let (a,b) = go l
            (e,d) = go r
        in (a + e , b + d)
	--(fst $ go l + fst $ go r + 1 , snd $ go l + snd $ go r)
	
	
type Endo a = a -> a

xor :: Bool -> Bool -> Bool
xor a b = a /= b

newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

instance Monoid Xor where
    mempty = Xor False
    mappend (Xor a) (Xor b)= Xor (xor a b)
	




class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

checkKey a (k,v) | a == k = True
                 | otherwise = False

	
-- instance MapLike ListMap where
    -- empty = ListMap []
    -- lookup k (ListMap l) = L.find (checkKey k) l
    -- insert k v (ListMap l) | any (checkKey k) l =ListMap $ map (\(a,b) -> if (k == a) then (a,v) else (a,b)) l
                           -- | otherwise = ListMap $ (k,v):l
    -- delete k (ListMap l) = ListMap $ L.deleteBy (\(a,_) (x,_) -> a == x) (k, undefined)    
    -- fromList [] = empty
    -- fromList l = ListMap l
    -- fromList ((k,v):xs) = insert k v (fromList xs)