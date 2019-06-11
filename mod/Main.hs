module Main where

import System.Environment

import Scan

main :: IO ()
main = do
    [f] <- getArgs
    s   <- readFile f
    print (scan s)
