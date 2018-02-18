{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Parser where

import Control.Applicative
import Data.Attoparsec.Text hiding (integerParser, decimalParser)
import Data.Char
import Data.Monoid
import Data.Text (strip, unpack, Text, pack)
import Data.Void
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

import Types

type MParser = Parsec Void Text

parseFeatureFromFile :: FilePath -> IO Feature
parseFeatureFromFile inputFile = do
  fileContents <- lines <$> readFile inputFile
  let nonEmptyLines = filter (not . isEmpty) fileContents
  let trimmedLines = map trim nonEmptyLines
  let finalString = pack $ unlines trimmedLines
  case parseOnly featureParser finalString of
    Left s -> error s
    Right feature -> return feature

featureParser :: Parser Feature
featureParser = do
  string "Feature: "
  title <- consumeLine
  maybeBackground <- optional backgroundParser
  scenarios <- many scenarioParser
  return $ Feature title maybeBackground scenarios

backgroundParser :: Parser Scenario
backgroundParser = do
  string "Background:"
  consumeLine
  statements <- many (parseStatement <* char '\n')
  examples <- (exampleTableParser <|> return (ExampleTable [] []))
  return $ Scenario "Background" statements examples

scenarioParser :: Parser Scenario
scenarioParser = do
  string "Scenario: "
  title <- consumeLine
  statements <- many (parseStatement <* char '\n')
  examples <- (exampleTableParser <|> return (ExampleTable [] []))
  return $ Scenario title statements examples

parseStatement :: Parser Statement
parseStatement =
  parseStatementLine "Given" <|>
  parseStatementLine "When" <|>
  parseStatementLine "Then" <|>
  parseStatementLine "And"

parseStatementLine :: Text -> Parser Statement
parseStatementLine signal = do
  string signal
  char ' '
  pairs <- many ((,) <$> nonBrackets <*> insideBrackets)
  finalString <- nonBrackets
  let (fullString, keys) = buildStatement pairs finalString
  return $ Statement fullString keys
  where
    buildStatement :: [(String, String)] -> String -> (String, [String])
    buildStatement [] last = (last, [])
    buildStatement ((str, key) : rest) rem =
      let (str', keys) = buildStatement rest rem
      in (str <> "<" <> key <> ">" <> str', key : keys)

nonBrackets :: Parser String
nonBrackets = many (satisfy (\c -> c /= '\n' && c /= '<'))

insideBrackets :: Parser String
insideBrackets = do
  char '<'
  key <- many letter
  char '>'
  return key

exampleTableParser :: Parser ExampleTable
exampleTableParser = do
  string "Examples:"
  consumeLine
  keys <- exampleColumnTitleLineParser
  valueLists <- many exampleLineParser
  return $ ExampleTable keys (map (zip keys) valueLists)

exampleColumnTitleLineParser :: Parser [String]
exampleColumnTitleLineParser = do
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

exampleLineParser :: Parser [Value]
exampleLineParser = do
  char '|'
  cells <- many cellParser
  char '\n'
  return cells
  where
    cellParser = do
      skipWhile nonNewlineSpace
      val <- valueParser
      skipWhile (not . barOrNewline)
      char '|'
      return val

valueParser :: Parser Value
valueParser =
  nullParser <|>
  boolParser <|>
  numberParser <|>
  stringParser

nullParser :: Parser Value
nullParser =
  (string "null" <|>
  string "NULL" <|>
  string "Null") *> pure ValueNull

boolParser :: Parser Value
boolParser = (trueParser *> pure (ValueBool True)) <|> (falseParser *> pure (ValueBool False))
  where
    trueParser = string "True" <|> string "true" <|> string "TRUE"
    falseParser = string "False" <|> string "false" <|> string "FALSE"

numberParser :: Parser Value
numberParser = ValueNumber <$> scientific

stringParser :: Parser Value
stringParser = (ValueString . unpack . strip) <$> takeTill (\c -> c == '|' || c == '\n')

valueParser' :: MParser Value
valueParser' = 
  nullParser' <|>
  boolParser' <|>
  numberParser' <|>
  stringParser'

nullParser' :: MParser Value
nullParser' = M.string' "null" >> return ValueNull

boolParser' :: MParser Value
boolParser' = (trueParser >> return (ValueBool True)) <|> (falseParser >> return (ValueBool False))
  where
    trueParser = M.string' "true"
    falseParser = M.string' "false"

numberParser' :: MParser Value
numberParser' = (ValueNumber . read) <$>
  (negativeParser <|> decimalParser <|> integerParser)
  where
    integerParser :: MParser String
    integerParser = M.try (some M.digitChar)

    decimalParser :: MParser String
    decimalParser = M.try $ do
      front <- many M.digitChar
      M.char '.'
      back <- some M.digitChar
      return $ front ++ ('.' : back)

    negativeParser :: MParser String
    negativeParser = M.try $ do
      M.char '-'
      num <- decimalParser <|> integerParser
      return $ '-' : num

stringParser' :: MParser Value
stringParser' = (ValueString . trim) <$>
  many (M.satisfy (not . barOrNewline))

isEmpty :: String -> Bool
isEmpty = all isSpace

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

consumeLine :: Parser String
consumeLine = do
  str <- Data.Attoparsec.Text.takeWhile (/= '\n')
  char '\n'
  return (unpack str)
