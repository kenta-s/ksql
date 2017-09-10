module Main where
import SQLParsers.Parser as Parser

main :: IO ()
main = putStrLn (Parser.parse "SELECT * FROM USERS;")
