module Main where

import System.Environment

import AST
import Desugar
import Parse
import SAST
import Type

main :: IO ()
main = do
    [f] <- getArgs
    s   <- readFile f
    print (parseSAST s)
