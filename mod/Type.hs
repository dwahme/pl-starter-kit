module Type
(
    Type
)
where

data Type
    = Unit
    | Lam Type Type
    deriving (Show)
