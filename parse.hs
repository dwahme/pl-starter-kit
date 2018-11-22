
module Parse (
    TopLevelExp(..),
    BoolExp(..),
    parse
) where

import           Control.Applicative          hiding (many, optional)
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Ord
import           Text.ParserCombinators.ReadP

-- The top level expressions in YorkSlang
-- Supported expressions:
    -- Boolin expressions (boolean expressions)
    -- More to be implemented later
data TopLevelExp
    = BoolTLE BoolExp
    deriving (Eq, Show)

-- A BoolExp is a Boolin or combination of 
-- a boolean operator and its arguments
data BoolExp
    = Boolin Boolin
    | Nah    BoolExp
    | Yurrd  BoolExp BoolExp
    | Uh     BoolExp BoolExp
    deriving (Eq, Show)

-- YorkSlang's version of a boolean
-- Wavy: True
-- Wack: False
-- Whatever: Maybe (a special type of Boolin)
data Boolin 
    = Wavy
    | Wack
    | Whatever
    deriving (Eq, Show)

-- Boolin Data types
wavy, wack, whatever :: String
wavy     = "Wavy"
wack     = "Wack"
whatever = "Whatever"

-- Boolin Operators
nah, yurrd, uh :: String
nah   = "Nah"
yurrd = "Yurrd"
uh    = "Uh"

means :: String -> a -> ReadP a
name `means` meaning = skipSpaces *> string name *> pure meaning

parens :: ReadP a -> ReadP a
parens = between (skipSpaces *> char '(') (skipSpaces *> char ')')

prefix :: ReadP a -> ReadP (a -> a) -> ReadP a
prefix p op = result where
  result = p <++ (op <*> result)

parseBoolin :: ReadP BoolExp
parseBoolin = do
    skipSpaces
    Boolin <$> wavy `means` Wavy +++ wack `means` Wack +++ whatever `means` Whatever

parseBoolSingleOp :: ReadP BoolExp
parseBoolSingleOp = do
    skipSpaces
    op <- nah `means` Nah
    exp1 <- parseBoolExp
    return $ op exp1

parseBoolDoubleOp :: ReadP BoolExp
parseBoolDoubleOp = do
    skipSpaces
    op <- (yurrd `means` Yurrd +++ uh `means` Uh)
    exp1 <- parseBoolExp
    exp2 <- parseBoolExp
    return $ op exp1 exp2

parseBoolOp :: ReadP BoolExp
parseBoolOp = parseBoolSingleOp +++ parseBoolDoubleOp

parseBoolExp :: ReadP BoolExp
parseBoolExp = exp where
    exp = parseBoolOp <++ parens exp <++ parseBoolin

-- Generates a bool top level expression parser
parseBoolTLE :: ReadP TopLevelExp
parseBoolTLE = BoolTLE <$> parseBoolExp

-- Parses a top level expression
parseTLE :: ReadP TopLevelExp
parseTLE = do
    tle <- parseBoolTLE
    skipSpaces
    return tle

-- Run the parser on a given string
parse :: String -> Either String TopLevelExp
parse str =
    case (completeParses, incompleteParses) of
    ([(result, "")], _  ) -> Right result  -- Only complete result.
    ([]            , [] ) -> Left $ "No parse."
    ([]            , _:_) -> Left $ "Incomplete parse. Unparsed: " ++ show leastRemaning
    (_:_           , _  ) -> Left $ "Ambiguous parse: " ++ show completeParses
    where
    parses = readP_to_S parseTLE str
    (completeParses, incompleteParses) =
        partition (\(_, remaining) -> remaining == "") parses
    leastRemaning = minimumBy (comparing length) . map snd $ incompleteParses
    