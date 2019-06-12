module AST 
(
    Ast,
    isVal
)
where

data Ast
    = Var String
    | Abs String Ast
    | App Ast Ast
    | Unit
    deriving (Show)

isVal :: Ast -> Bool
isVal (Abs _ _) = True
isVal Unit = True
isVal _ = False
