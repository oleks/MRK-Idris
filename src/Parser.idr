module Parser

import Ast
import SimpleParse
import Data.String

parseIntegral : Parser String
parseIntegral = munch (\c => c `elem` ['0'..'9'])

parsePoints : Parser Double
parsePoints = do
  is <- parseIntegral
  fs <- (char '.' *> parseIntegral) <|> pure "0"
  case (String.parseDouble (is ++ "." ++ fs)) of
    Just x => pure x
    _ => reject

lineToken : Parser a -> Parser a
lineToken p = munch (\c => c `elem` [' ', '\t']) *> p

munchTillExcl : Char -> Parser String
munchTillExcl c = munch (/= c) <* char c

parseHeader : Nat -> Parser Header
parseHeader depth = do
  let mark = Stream.take depth $ repeat '#'
  string $ pack mark
  char ' '
  title <- lineToken $ munchTillExcl ':'

  points <- lineToken $ parsePoints
  lineToken $ char '/'
  maxPoints <- lineToken $ parsePoints

  pure $ MkHeader title points maxPoints

parseMood : Parser Mood
parseMood = choice
  [ char '+' *> pure Positive
  , char '-' *> pure Negative
  , char '*' *> pure Neutral
  , char '?' *> pure Impartial
  ]

parseLine : Parser String
parseLine = munchTillExcl '\n'

parseLines : String -> Parser (List String)
parseLines indent = many $ string indent *> parseLine

export
parseComment : Nat -> Parser Comment
parseComment depth = do
  let realdepth = depth * 2
  let indent = pack $ Stream.take realdepth $ repeat ' '
  string indent
  mood <- parseMood
  char ' '
  first <- parseLine
  rest <- parseLines (indent ++ "  ")
  pure $ MkComment mood (first :: rest) []

public export
data ParseError a
  = NoParses
  | IncompleteParse (a, String)
  | AmbiguousGrammar (List (a, String))
  | IOError FileError

export
Show a => Show (ParseError a) where
  show NoParses = "NoParses"
  show (IncompleteParse (a, s)) = "IncompleteParse (" ++ show a ++ ", " ++ s ++ ")"
  show (AmbiguousGrammar as) = "AmbiguousGrammar " ++ show as
  show (IOError e) = "IOError " ++ show e

export
parseFile : Parser a -> String -> IO (Either (ParseError a) a)
parseFile p path = do
  contents <- readFile path
  pure $ case contents of
    Right s => case (parse p s) of
      [(a, "")] => Right a
      [] => Left NoParses
      [(a, s)] => Left $ IncompleteParse (a, s)
      ps => Left $ AmbiguousGrammar ps
    Left e => Left $ IOError e
