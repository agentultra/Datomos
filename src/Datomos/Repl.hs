module Datomos.Repl where

import Control.Monad
import System.IO

read_ :: IO String
read_ = putStr "DB> "
        >> hFlush stdout
        >> getLine

eval_ :: String -> String
eval_ = id

print_ :: String -> IO ()
print_ = putStrLn

repl :: IO ()
repl = do
  input <- read_

  unless (input == ":quit") $
    print_ (eval_ input) >> repl
