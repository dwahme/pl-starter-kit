module SAST 
(
    SAST(Var, Abs, App, Unit)
)
where

data SAST
    = Var String
    | Abs String SAST
    | App SAST SAST
    | Unit
    deriving (Show)
