module Main

import Ast
import SimpleParse
import Parser

main : IO ()
main = do
  content <- parseFile (parseComment 0) "../test/golden/positive-comment.mrk"
  case content of
    Right p => putStrLn $ show p
    Left e => putStrLn $ show e
