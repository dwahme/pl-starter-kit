module Parse 
(
    parseAST
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

parser :: Parser AST
parser = do
    ast <- getAST
    eof
    return ast

parseAST :: String -> AST
parseAST s = case parse scanner "" s of
    Left err -> error $ show err
    Right ast -> ast
