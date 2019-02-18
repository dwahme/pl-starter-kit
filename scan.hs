

module Scan (
    scan
) where

import           Control.Applicative          hiding (many, optional)
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Ord

scan :: String -> IO String
scan pathStr = do
    path <- pathStr

