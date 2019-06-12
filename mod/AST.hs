module AST 
(
    AST(Var, Abs, App, Unit),
    isVal
)
where

data AST
    = Var String
    | Abs String AST
    | App AST AST
    | Unit
    deriving (Show)

isVal :: AST -> Bool
isVal (Abs _ _) = True
isVal Unit = True
isVal _ = False
