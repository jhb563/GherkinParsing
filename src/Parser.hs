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
  Feature <$> parseFeatureTitleLine <*> optional backgroundParser <*> many scenarioParser <* many anySym

parseFeatureTitleLine :: RE Char String
parseFeatureTitleLine = string "Feature: " *> readThroughEndOfLine

backgroundParser :: RE Char Scenario
backgroundParser = (string "Background:" *> readThroughEndOfLine) *>
  (Scenario "Background" <$> many (parseStatement <* sym '\n')
  <*> (exampleTableParser <|> pure (ExampleTable [] [])))

scenarioParser :: RE Char Scenario
scenarioParser = Scenario <$> 
  (string "Scenario: " *> readThroughEndOfLine)
  <*> many (parseStatement <* sym '\n')
  <*> (exampleTableParser <|> pure (ExampleTable [] []))

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

exampleTableParser :: RE Char ExampleTable
exampleTableParser = buildExampleTable <$>
  (string "Examples:" *> readThroughEndOfLine *>
  exampleColumnTitleLineParser) <*>
  many exampleLineParser
  where
    buildExampleTable :: [String] -> [[Value]] -> ExampleTable
    buildExampleTable keys valueLists = ExampleTable keys (map (zip keys) valueLists)

exampleColumnTitleLineParser :: RE Char [String]
exampleColumnTitleLineParser = sym '|' *> many cellParser <* sym '\n'
  where
    cellParser = many isNonNewlineSpace *> many (psym isAlpha) <* readThroughBar

exampleLineParser :: RE Char [Value]
exampleLineParser = sym '|' *> many cellParser <* sym '\n'
  where
    cellParser = many isNonNewlineSpace *> valueParser <* readThroughBar

isNonNewlineSpace :: RE Char Char
isNonNewlineSpace = psym (\c -> isSpace c && c /= '\n')

valueParser :: RE Char Value
valueParser =
  nullParser <|>
  boolParser <|>
  numberParser <|>
  stringParser

nullParser :: RE Char Value
nullParser =
  (string "null" <|>
  string "NULL" <|>
  string "Null") *> pure ValueNull

boolParser :: RE Char Value
boolParser = trueParser *> pure (ValueBool True) <|> falseParser *> pure (ValueBool False)
  where
    trueParser = string "True" <|> string "true" <|> string "TRUE"
    falseParser = string "False" <|> string "false" <|> string "FALSE"

numberParser :: RE Char Value
numberParser = (ValueNumber . read) <$>
  (negativeParser <|> decimalParser <|> integerParser)
  where
    integerParser = some (psym isNumber)
    decimalParser = combineDecimal <$> many (psym isNumber) <*> sym '.' <*> some (psym isNumber)
    negativeParser = (:) <$> sym '-' <*> (decimalParser <|> integerParser)

    combineDecimal :: String -> Char -> String -> String
    combineDecimal base point decimal = base ++ (point : decimal)

stringParser :: RE Char Value
stringParser = (ValueString . trim) <$> readUntilBar

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
