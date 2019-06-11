module Scan 
(
    Tok,
    scan
)
where

data Tok
    = LParen
    | RParen
    deriving (Show)

scan :: String -> [Tok]
scan [] = []
scan ('(':s) = LParen : (scan s)
scan (')':s) = RParen : (scan s)
scan (c:_) = error $ "Illegal character" ++ [c] ++ " in program"
