module Main where
import SQLParsers.Parser as Parser

main :: IO ()
main = do
  putStrLn "ksql > "
  sql <- getLine
  putStrLn $ Parser.parse sql
