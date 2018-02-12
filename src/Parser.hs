{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Parser where

import Data.Attoparsec.Text (Parser, scientific, takeTill, char, skipWhile, letter)
import qualified Data.Attoparsec.Text as A
import Data.Char
import Data.Monoid
import Text.Regex.Applicative
import Data.Text (strip, unpack)

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

exampleTableParser' :: Parser ExampleTable
exampleTableParser' = do
  A.string "Examples:"
  consumeLine
  keys <- exampleColumnTitleLineParser'
  valueLists <- many exampleLineParser'
  return $ ExampleTable keys (map (zip keys) valueLists)

exampleColumnTitleLineParser' :: Parser [String]
exampleColumnTitleLineParser' = do
  char '|'
  cells <- many cellParser
  char '\n'
  return cells
  where
    cellParser = do
      skipWhile nonNewlineSpace
      val <- many letter
      skipWhile (not . barOrNewline)
      char '|'
      return val

exampleLineParser' :: Parser [Value]
exampleLineParser' = do
  char '|'
  cells <- many cellParser
  char '\n'
  return cells
  where
    cellParser = do
      skipWhile nonNewlineSpace
      val <- valueParser'
      skipWhile (not . barOrNewline)
      char '|'
      return val

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

valueParser' :: Parser Value
valueParser' =
  nullParser' <|>
  boolParser' <|>
  numberParser' <|>
  stringParser'

nullParser' :: Parser Value
nullParser' =
  (A.string "null" <|>
  A.string "NULL" <|>
  A.string "Null") >> return ValueNull

boolParser' :: Parser Value
boolParser' = (trueParser >> return (ValueBool True)) <|> (falseParser >> return (ValueBool False))
  where
    trueParser = A.string "True" <|> A.string "true" <|> A.string "TRUE"
    falseParser = A.string "False" <|> A.string "false" <|> A.string "FALSE"

numberParser' :: Parser Value
numberParser' = ValueNumber <$> scientific

stringParser' :: Parser Value
stringParser' = (ValueString . unpack . strip) <$> takeTill (\c -> c == '|' || c == '\n')

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

barOrNewline :: Char -> Bool
barOrNewline c = c == '|' || c == '\n'

nonNewlineSpace :: Char -> Bool
nonNewlineSpace c = isSpace c && c /= '\n'

consumeLine :: Parser ()
consumeLine = skipWhile (/= '\n') >> char '\n' >> return ()
