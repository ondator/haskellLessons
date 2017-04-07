{-# LANGUAGE FlexibleInstances #-}
module Warhammer where

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab x | ((doesEnrageMork x) && (doesEnrageGork x)) = stomp (stab x)
				  | doesEnrageMork x = stomp x
				  | doesEnrageGork x = stab x
				  | otherwise = x
				  
instance KnownToGork [Char] where
	stomp a = a ++ "Gork"
	doesEnrageGork a = True
	
instance KnownToMork [Char] where
	stab a = a ++ "Mork"
	doesEnrageMork a = True
	
instance KnownToGorkAndMork [Char] where	