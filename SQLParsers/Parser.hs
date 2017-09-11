module SQLParsers.Parser
( parse
) where

import Data.Char

parse :: String -> String
parse sql
    | firstWord == "SELECT" = "SQL received. This function is not implemented yet."
    | otherwise = error "Invalid SQL detected"
    where firstWord = [toUpper x | x <- takeWhile ( /= ' ' ) sql]
