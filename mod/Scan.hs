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
    try spaces
    string "()"
    return Unit

inParens :: Parser AST
inParens = do
    try spaces
    char '('
    exp <- scanner
    try spaces
    char ')'
    return exp

scanner :: Parser AST
scanner = do
    ast <- try getUnit <|> inParens
    notFollowedBy $ anyToken
    return ast

scan :: String -> AST
scan s = case parse scanner "" s of
    Left err -> error $ show err
    Right ast -> ast
