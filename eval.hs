module Eval (
  EvalResult,
  eval
) where

import           Control.Applicative
import           Parse

-- Error message or result.
type EvalResult = Either String Boolin

type Bindings = [(Name, Number)]


-- Return wrapped list of bare results if all inputs are Right.
-- Otherwise, returns the first Left error message.
allRight :: [EvalResult] -> Either String [Number]
allRight = foldr (liftA2 (:)) (Right [])


-- Returns either an error string or a resulting integer.
eval :: TopLevelExp -> EvalResult
eval (BoolTLE mExp)              = evalMathExp [] mathExp
-- eval (LetTLE names mathExps mainExp)
--   | length names == length mathExps = performEval
--   | otherwise                       = Left errorMsg
--   where
--     performEval = do
--       bindings <- bindingsOrError
--       evalBoolExp bindings mainExp
--     bindingsOrError =
--       zip names <$> (allRight . map (evalBoolExp []) $ mathExps)
--     errorMsg =
--       "must assign " ++ show (length names) ++
--       " names but given " ++ show (length mathExps) ++
--       " expressions"

-- Bindings are the variables in scope.
--
-- Returns either an error string or a resulting number.
evalBoolExp :: Bindings -> BoolExp -> EvalResult
evalBoolExp bindings exp =
  let
    recurse        = evalBoolExp bindings
    unOp op e      = op <$> recurse e
    binOp op e1 e2 = liftA2 op (recurse e1) (recurse e2)
  in
  case exp of
    Boolin e -> Right e
    Nah    e -> evalNah e
    _        -> Whatever
