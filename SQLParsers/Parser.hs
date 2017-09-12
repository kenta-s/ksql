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
    | firstWord == "SELECT" = show tree
    | otherwise = error "Invalid SQL detected"
    where firstWord = [toUpper x | x <- takeWhile ( /= ' ' ) sql]
          targets = extractTargets sql
          tree = insertTarget targets $ SyntaxTree {targetList=[], fromClause=[]}

insertTarget :: [String] -> SyntaxTree -> SyntaxTree
insertTarget targets tree =
    SyntaxTree {targetList = newTargetList, fromClause = newFromClause}
    where newTargetList = targetList tree ++ targets
          newFromClause = fromClause tree

extractTargets :: String -> [String]
-- TODO: fix the behavior
extractTargets sql =
    [dropWhile (== ' ') $ dropWhile (/= ' ') sql]
