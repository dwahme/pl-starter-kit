module Parse 
(
    parseSAST
)
where

import Text.Parsec
import Text.Parsec.String (Parser)
import SAST

getUnit :: Parser SAST
getUnit = do
    between (char '(') (char ')') $ try spaces
    return Unit

getVar :: Parser SAST
getVar = do
    fst <- letter <|> char '_'
    rst <- many (alphaNum <|> char '_')
    return $ Var (fst:rst)

getLambda :: Parser SAST
getLambda = do
    char '\\'
    try spaces
    Var var <- getVar
    between (try spaces) (try spaces) (string "=>")
    exp <- getSAST
    return $ Abs var exp

inParens :: Parser SAST
inParens = do
    sast <- between (char '(') (char ')') getSAST
    return sast

getSAST :: Parser SAST
getSAST = do
    try spaces
    exp:exps <- flip sepEndBy1 spaces $ 
        try getUnit <|> 
        try inParens <|> 
        try getLambda <|> 
        getVar
    try spaces
    return $ foldl (\a b -> App a b) exp exps

parser :: Parser SAST
parser = do
    sast <- getSAST
    eof
    return sast

parseSAST :: String -> SAST
parseSAST s = case parse parser "" s of
    Left err -> error $ show err
    Right sast -> sast
