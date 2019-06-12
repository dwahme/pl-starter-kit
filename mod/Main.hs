module Main where

import System.Environment

import AST
import Scan

main :: IO ()
main = do
    [f] <- getArgs
    s   <- readFile f
    print (scan s)
