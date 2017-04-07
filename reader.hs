module Monads.Reader where

data Reader r a = Reader { runReader :: (r -> a) }

instance Monad (Reader r) where
  return x = Reader $ \_ -> x
  m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r
  

local' :: (r -> r1) -> Reader r1 a -> Reader r a
local' f m = Reader $ \e -> runReader m (f e)



type User = String
type Password = String
type UsersTable = [(User, Password)]

usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = Reader (map fst . filter isBad) where
                        isBad (u,"123456") = True
                        isBad _ = False
  