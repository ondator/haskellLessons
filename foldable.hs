{-# LANGUAGE TypeOperators#-}
module Foldable where

import Data.Foldable hiding (concat)
import Prelude hiding (foldr, foldl)
import Data.Monoid

data Triple a = Tr a a a  deriving (Eq,Show)

instance Foldable Triple where
    foldr f init (Tr a b c) = a `f` (b `f` (c `f` init))
    foldl f init (Tr a b c) = ((init `f` a) `f` b) `f` c
	
	
data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)

newtype Preorder a   = PreO   (Tree a)    deriving (Eq, Show)
newtype Postorder a  = PostO  (Tree a)    deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a)    deriving (Eq, Show)

instance Foldable Preorder where
    foldr f ini (PreO Nil) = ini
    foldr f ini (PreO (Branch l x r)) = f x (foldr f (foldr f ini (PreO r)) (PreO l))
	
instance Foldable Tree where
    foldr f ini Nil = ini
    foldr f ini (Branch l x r) = foldr f (f x (foldr f ini r)) l 
	

instance Foldable Postorder where
    foldr f ini (PostO Nil) = ini
    foldr f ini (PostO (Branch l x r)) = foldr f (foldr f (f x ini) (PostO r)) (PostO l) 
	
instance Foldable Levelorder where
    foldr f ini (LevelO Nil) = ini
    foldr f ini (LevelO tree) = foldr f ini (tbf [tree]) where
        tbf [] = []
        tbf xs = map nodeValue xs ++ tbf (concat (map leftAndRightNodes xs))
        nodeValue (Branch _ a _) = a
        leftAndRightNodes (Branch Nil _ Nil) = []
        leftAndRightNodes (Branch Nil _ b)   = [b]
        leftAndRightNodes (Branch a _ Nil)   = [a]
        leftAndRightNodes (Branch a _ b)     = [a,b]
		
		

mkEndo :: Foldable t => t (a -> a) -> Endo a
mkEndo = foldr (mappend . Endo) mempty


infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show)

instance (Foldable f, Foldable g) => Foldable ((|.|) f g) where
    foldr f ini cmp = 
	--foldr f ini $ foldr mappend mempty $ getCmps cmp