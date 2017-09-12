module SQLParsers.Parser
( parse
) where

import Data.Char

data SyntaxTree = SyntaxTree {
    targetList :: [String],
    fromClause :: [String]
} deriving (Show)

parse :: String -> String
parse sql
    | firstWord == "SELECT" = show SyntaxTree {targetList=targets, fromClause=fromClause}
    | otherwise = error "Invalid SQL detected"
    where firstWord = map toUpper $ head $ words sql
          targets = extractTargets sql
          fromClause = extractFromClause sql

extractTargets :: String -> [String]
extractTargets sql =
    [dropSymbol x | x <- takeWhile (/= "FROM") $ words $ map toUpper sql, x /= "SELECT"]

extractFromClause :: String -> [String]
extractFromClause sql =
    map dropSymbol afterFrom
    where afterFrom = tail [x | x <- dropWhile (/= "FROM") $ words $ map toUpper sql]

dropSymbol :: String -> String
dropSymbol str
    | lastChar == ',' = init str
    | lastChar == ';' = init str
    | otherwise       = str
    where lastChar = last str
