{-# LANGUAGE TupleSections #-}
module Parser where

import Data.Char
import Data.Monoid
import Text.Regex.Applicative

import Types

parseFeatureFromFile :: FilePath -> IO Feature
parseFeatureFromFile inputFile = do
  fileContents <- lines <$> readFile inputFile
  let nonEmptyLines = filter (not . isEmpty) fileContents
  let trimmedLines = map trim nonEmptyLines
  let finalString = unlines trimmedLines
  case parseFeature finalString of
    Nothing -> error "Couldn't parse!"
    Just feature -> return feature

parseFeature :: String -> Maybe Feature
parseFeature input = input =~
  Feature <$> parseFeatureTitleLine <*> optional backgroundParser <*> many parseScenario <* many anySym

parseFeatureTitleLine :: RE Char String
parseFeatureTitleLine = string "Feature: " *> readThroughEndOfLine

backgroundParser :: RE Char Scenario
backgroundParser = (string "Background:" *> readThroughEndOfLine) *>
  (Scenario "Background" <$> many (parseStatement <* sym '\n')
  <*> (parseExamples <|> pure (ExampleTable [] [])))

parseScenario :: RE Char Scenario
parseScenario = Scenario <$> 
  (string "Scenario: " *> readThroughEndOfLine)
  <*> many (parseStatement <* sym '\n')
  <*> (parseExamples <|> pure (ExampleTable [] []))

parseStatement :: RE Char Statement
parseStatement =
  parseStatementLine "Given" <|>
  parseStatementLine "When" <|>
  parseStatementLine "Then" <|>
  parseStatementLine "And"

parseStatementLine :: String -> RE Char Statement
parseStatementLine signal = finalizeStatement <$>
  (string signal *> sym ' ' *>
  (buildStatement <$> many ((,) <$> nonBrackets <*> insideBrackets) <*> nonBrackets))
  where
    buildStatement :: [(String, String)] -> String -> (String, [String])
    buildStatement [] last = (last, [])
    buildStatement ((str, key) : rest) rem =
      let (str', keys) = buildStatement rest rem
      in (str <> "<" <> key <> ">" <> str', key : keys)

    finalizeStatement :: (String, [String]) -> Statement
    finalizeStatement (regex, variables) = Statement regex variables

nonBrackets :: RE Char String
nonBrackets = many (psym (\c -> c /= '\n' && c /= '<'))

insideBrackets :: RE Char String
insideBrackets = sym '<' *> many (psym (/= '>')) <* sym '>'

parseExamples :: RE Char ExampleTable
parseExamples = convertExampleBuilder <$>
  (string "Examples:" *>
  readThroughEndOfLine *>
  ((,) <$> exampleColumnTitleLine <*> many exampleLine))

convertExampleBuilder :: ([String], [[Value]]) -> ExampleTable
convertExampleBuilder (keys, values) = ExampleTable keys examples
  where
    examples = map (zip keys) values

exampleColumnTitleLine :: RE Char [String]
exampleColumnTitleLine =
  sym '|' *> many readExampleColumnTitle <* sym '\n'

readExampleColumnTitle :: RE Char String
readExampleColumnTitle = trim <$> readThroughBar

exampleLine :: RE Char [Value]
exampleLine =
  sym '|' *> many readExample <* sym '\n'

readExample :: RE Char Value
readExample = spaces *> parseValue <* readThroughBar

parseValue :: RE Char Value
parseValue =
  string "null" *> pure ValueNull <|>
  string "NULL" *> pure ValueNull <|>
  string "Null" *> pure ValueNull <|>
  string "false" *> pure (ValueBool False) <|>
  string "False" *> pure (ValueBool False) <|>
  string "true" *> pure (ValueBool True) <|>
  string "True" *> pure (ValueBool True) <|>
  (ValueNumber . read <$> some (psym isNumber)) <|>
  (ValueString . trim <$> readUntilBar)

isEmpty :: String -> Bool
isEmpty = all isSpace

spaces :: RE Char ()
spaces = many (psym (\c -> isSpace c && c /= '\n')) *> pure ()

readThroughBar :: RE Char String
readThroughBar = readUntilBar <* sym '|'

readUntilBar :: RE Char String
readUntilBar = many (psym (\c -> c /= '|' && c /= '\n'))

readUntilEndOfLine :: RE Char String
readUntilEndOfLine = many (psym (/= '\n'))

readThroughEndOfLine :: RE Char String
readThroughEndOfLine = readUntilEndOfLine <* sym '\n'

trim :: String -> String
trim input = reverse flippedTrimmed
  where
    trimStart = dropWhile isSpace input
    flipped = reverse trimStart
    flippedTrimmed = dropWhile isSpace flipped
