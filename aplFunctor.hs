module AplFunctor where
import Control.Applicative

newtype Arr2 e1 e2 a = Arr2 {getArr2 :: e1 -> e2 -> a}
newtype Arr3 e1 e2 e3 a = Arr3 {getArr3 :: e1 -> e2 -> e3 -> a}


instance Functor (Arr2 e1 e2) where
 fmap f arr = Arr2 $ \x y -> f $ a x y where
          a = \e1 e2 -> (getArr2 arr) e1 e2

instance Functor (Arr3 e1 e2 e3) where
 fmap f arr = Arr3 $ \x y z -> f $ a x y z where
              a = \e1 e2 e3 -> (getArr3 arr) e1 e2 e3



data Triple a = Tr a a a deriving (Eq,Show)

instance Functor Triple where
  fmap f (Tr x y z) = Tr (f x) (f y) (f z)  
  
instance Applicative Triple where
  pure a = Tr a a a
  (<*>) (Tr f g h) (Tr x y z) = Tr (f x) (g y) (h z)  
  

f >$< xs = getZipList $ f <$> ZipList xs
xs >*< ys = getZipList $ ZipList xs <*> ZipList ys


divideList' :: (Show a, Fractional a) => [a] -> (String,a)
divideList' []     = ("1.0", 1)
divideList' (x:xs) = (/) <$> (("<-" ++ show x ++ "/"),x) <*> (divideList' xs)

instance Applicative (Arr2 e1 e2) where
    pure a = Arr2 $ \e1 e2 -> a
    (<*>) a1 a2 = Arr2 (\e1 e2 -> (getArr2 a1) e1 e2 ((getArr2 a2) e1 e2))


instance Applicative (Arr3 e1 e2 e3) where
    pure a = Arr3 $ \e1 e2 e3 -> a
    (<*>) a1 a2 = Arr3 (\e1 e2 e3-> (getArr3 a1) e1 e2 e3 ((getArr3 a2) e1 e2 e3))