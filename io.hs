module Main where

import System.Directory

main :: IO ()
main = do
  putStrLn "What is your name?"
  putStr "Name: "
  name <- getLine
  if (name == "") then main else putStrLn $ "Hi, " ++ name ++ "!"
  
  
main' :: IO ()
main' = do
  putStr "Substring: "
  pattern <- getLine
  files <- getDirectoryContents "."
  if (pattern == "") then putStrLn "Canceled" else deleteByPattern pattern files where
              deleteByPattern _ [] = return ()
              deleteByPattern p (f:files) | substring p f = do
                                                            removeFile f
                                                            putStrLn $ "Removing file: " ++ f
                                                            deleteByPattern p files
                                          | otherwise = deleteByPattern p files
              substring (x:xs) [] = False
              substring xs ys
                   | prefix xs ys = True
                   | substring xs (tail ys) = True
                   | otherwise = False

              prefix :: String -> String -> Bool
              prefix [] ys = True
              prefix (x:xs) [] = False
              prefix (x:xs) (y:ys) = (x == y) && prefix xs ys