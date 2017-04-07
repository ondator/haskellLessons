module ToString where

class Printable a where
	toString :: a -> [Char]
	
instance Printable Bool where
	toString True = "true"
	toString False = "false"
	
instance Printable () where
	toString x = "unit type"
	
instance (Printable a,Printable b) => Printable (a,b) where
	toString (a,b) = "(" ++ (toString a) ++ "," ++ (toString b) ++ ")"