module SAST 
(
    SAST
)
where

data SAST
    = Var String
    | Abs String SAST
    | App SAST SAST
    | Unit
    deriving (Show)
