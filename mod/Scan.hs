module Scan 
(
    scan
)
where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Error (messageString)
import AST

getUnit :: Parser AST
getUnit = do
    char '('
    try spaces
    char ')'
    return Unit

inParens :: Parser AST
inParens = do
    char '('
    exp <- getAST
    char ')'
    return exp

getAST :: Parser AST
getAST = do
    try spaces
    ast <- try getUnit <|> inParens
    try spaces
    return ast

scanner :: Parser AST
scanner = do
    ast <- getAST
    eof
    return ast

scan :: String -> AST
scan s = case parse scanner "" s of
    Left err -> error $ show err
    Right ast -> ast
