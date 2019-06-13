module Parse 
(
    parseAST
)
where

import Text.Parsec
import Text.Parsec.String (Parser)
import AST

getUnit :: Parser AST
getUnit = do
    between (char '(') (char ')') $ try spaces
    return Unit

getVar :: Parser AST
getVar = do
    fst <- letter
    rst <- many (alphaNum <|> char '_')
    return $ Var (fst:rst)

getLambda :: Parser AST
getLambda = do
    char '\\'
    try spaces
    Var var <- getVar
    try spaces
    string "=>"
    try spaces
    exp <- getAST
    return $ Abs var exp

inParens :: Parser AST
inParens = do
    ast <- between (char '(') (char ')') getAST
    return ast

getAST :: Parser AST
getAST = do
    try spaces
    ast <- try getUnit <|> inParens <|> getLambda <|> getVar
    try spaces
    return ast

parser :: Parser AST
parser = do
    ast <- getAST
    eof
    return ast

parseAST :: String -> AST
parseAST s = case parse parser "" s of
    Left err -> error $ show err
    Right ast -> ast
