{-# LANGUAGE TypeOperators#-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}

module Traversable where
import Control.Applicative

{- traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
traverse_ f = foldr ((*>) . f) (pure ())
-}

traverse2list :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]
traverse2list f = foldr ((\x y -> (:) <$> x <*> y) . f) (pure [])


data Triple a = Tr a a a  deriving (Eq,Show)

instance Functor Triple where
  fmap f (Tr x y z) = Tr (f x) (f y) (f z)  
  
instance Foldable Triple where
    foldr f init (Tr a b c) = a `f` (b `f` (c `f` init))
    foldl f init (Tr a b c) = ((init `f` a) `f` b) `f` c
  
instance Traversable Triple where
  traverse f (Tr x y z) = Tr <$> (f x) <*> (f y) <*> (f z)
  
  
data Result a = Ok a | Error String deriving (Eq,Show)

instance Functor Result where
   fmap _ (Error s) = Error s
   fmap f (Ok a) = Ok (f a)
   
instance Foldable Result where
   foldr _ ini (Error s) = ini
   foldr f ini (Ok a) = a `f` ini
   
   foldl _ ini (Error s) = ini
   foldl f ini (Ok a) = ini `f` a
   
instance Traversable Result where
   traverse _ (Error s) = pure (Error s)
   traverse f (Ok a) = Ok <$> (f a)
   
   
data Tree a = Nil | Branch (Tree a) a (Tree a)  deriving (Eq, Show)

instance Functor Tree where
    fmap _ Nil = Nil
    fmap f (Branch l a r) = Branch (fmap f l) (f a) (fmap f r)

instance Foldable Tree where
    foldr f ini Nil = ini
    foldr f ini (Branch l x r) = f x (foldr f (foldr f ini r) l)

instance Traversable Tree where
    traverse _ Nil = pure Nil
    traverse f (Branch l a r) = Branch <$> (traverse f l) <*> (f a) <*> (traverse f r)


infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show) 



data OddC a = Un a | Bi a a (OddC a) deriving (Eq, Show)

instance Functor OddC where
 fmap f (Un a) = Un (f a)
 fmap f (Bi a b c) = Bi (f a) (f b) (fmap f c)

instance Foldable OddC where
 foldr f ini (Un a) = f a ini
 foldr f ini (Bi a b c) = f a (f b (foldr f ini c))
 
 
instance Traversable OddC where
 traverse f (Un a) = Un <$> f a
 traverse f (Bi a b c) = Bi <$> f a <*> f b <*> traverse f c
 
 
newtype Temperature a = Temperature Double
  deriving (Num,Show,Eq,Fractional)

data Celsius
data Fahrenheit 
data Kelvin

comfortTemperature :: Temperature Celsius
comfortTemperature = Temperature 23

c2f :: Temperature Celsius -> Temperature Fahrenheit
c2f (Temperature c) = Temperature (1.8 * c + 32)

k2c :: Temperature Kelvin -> Temperature Celsius
k2c (Temperature a) = Temperature (a - 273.15)