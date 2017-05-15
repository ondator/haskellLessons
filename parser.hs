module Parser where

import Control.Applicative
-- import Text.Parsec 
-- import Text.ParserCombinators.Parsec

ignoreBraces bO bC p = bO *> p <* bC

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }


instance Functor Prs where
  fmap f p = Prs fun where
       fun "" = Nothing
       fun s = Just (f a, t) where
               Just (a,t) = runPrs p s
  
-- instance Functor Prs where
  -- fmap _ Nothing = Nothing
  -- fmap f (Just (a, t)) = Just (f a, t)

anyChr :: Prs Char
anyChr = Prs f where
  f "" = Nothing
  f (c:cs) = Just (c, cs)
  
  
instance Applicative Prs where
  pure a = Prs (\s -> Just (a,s))
  pf <*> pv = Prs f where
                  f s = do
                     (g, s') <- runPrs pf s
                     (a, s'') <- runPrs pv s'
                     return (g a, s'')


newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE pr = PrsE f where
    f "" = Left "unexpected end of input"
    f (c:cs) | pr c = Right (c, cs)
             | otherwise = Left ("unexpected " ++ (c : ""))

charE :: Char -> PrsE Char
charE c = satisfyE (== c) 


instance Functor PrsE where
   fmap f p = PrsE fun where
        fun "" = Left "unexpected end of input"
        fun s = Right (f a, s') where
                Right (a, s') = runPrsE p s


instance Applicative PrsE where
    pure a = PrsE (\s->Right(a,s))
    pf <*> pv = PrsE fun where
       fun s = do
           (g,s')  <- runPrsE pf s
           (a,s'') <- runPrsE pv s'
           return (g a, s'')


instance Alternative Prs where
    empty = Prs (\_ -> Nothing)
    pa <|> pb = Prs fun where
           fun s = case runPrs pa s of
               Nothing -> runPrs pb s
               _ -> runPrs pa s
  

satisfy :: (Char -> Bool) -> Prs Char
satisfy pr = Prs f where
        f "" = Nothing
        f (c:cs) | pr c = Just (c, cs)
                 | otherwise = Nothing
 
char :: Char -> Prs Char
char c = satisfy (==c)


prsOk p = (pure (:) <*> p <*> pure []) <|> pureM [] where
          pureM a = Prs (\(c:cs)->Just(a,cs))

many1 :: Prs a -> Prs [a]
many1 p = (++) <$> prsOk p <*> many1 p <|> pure []