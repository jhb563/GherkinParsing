{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory
import Test.Hspec

import Data.Attoparsec.Text (IResult(..), parseOnly, Parser)
import Data.Text (Text, pack)
import Parser
import Text.Regex.Applicative
import Types

import TestLoginFeatures
import TestRegistrationFeatures
import TestWithdrawalFeatures

main :: IO ()
main = do
  currentDir <- getCurrentDirectory 
  let featureDir = currentDir ++ testFeaturesExtension
  let regFile = featureDir ++ regFeatureFile
  let loginFile = featureDir ++ loginFeatureFile
  let withdrawalFile = featureDir ++ withdrawalFeatureFile
  hspec $ do
    registrationSpec regFile
    loginSpec loginFile
    withdrawalSpec withdrawalFile
    exampleTableSpec
    valueSpec
    statementSpec

shouldMatch :: (Show a, Eq a) => RE Char a -> String -> a -> IO ()
shouldMatch parser input result = input =~ parser `shouldBe` Just result

shouldMatch' :: (Show a, Eq a) => Parser a -> Text -> a -> IO ()
shouldMatch' parser input result = parseOnly parser input `shouldBe` (Right result)

registrationSpec :: FilePath -> Spec
registrationSpec featureFile = describe "Parse Registration Feature" $ do
  it "Should match our expected feature" $
    parseFeatureFromFile featureFile `shouldReturn` registrationFeature

loginSpec :: FilePath -> Spec
loginSpec featureFile = describe "Parse Login Feature" $ do
  it "Should match our expected feature" $
    parseFeatureFromFile featureFile `shouldReturn` loginFeature

withdrawalSpec :: FilePath -> Spec
withdrawalSpec featureFile = describe "Parse Withdrawal Feature" $ do
  it "Should match our expected feature" $
    parseFeatureFromFile featureFile `shouldReturn` withdrawalFeature

exampleTableSpec :: Spec
exampleTableSpec = describe "Parse Simple Example Table" $ do
  it "Should match the expected table" $
    shouldMatch' exampleTableParser' (pack sampleExamples) producedExample

valueSpec :: Spec
valueSpec = describe "Parse Single Values" $ do
  context "When the value is null" $
    it "Should be a null value" $
      shouldMatch' valueParser' "null" ValueNull
  context "When the value is a true boolean" $
    it "Should be ValueBool True" $
      shouldMatch' valueParser' "True" (ValueBool True)
  context "When the value is a false boolean" $
    it "Should be ValueBool False" $
      shouldMatch' valueParser' "false" (ValueBool False)
  context "When the value is a number" $
    it "Should be a value number" $
      shouldMatch' valueParser' "1234" (ValueNumber 1234)
  context "When the value is a decimal number" $
    it "Should be a value number" $
      shouldMatch' valueParser' "1234.5123" (ValueNumber 1234.5123)
  context "When the value is anything else" $
    it "Should be a value string" $
      shouldMatch' valueParser' "ABCD1234!?" (ValueString "ABCD1234!?")

sampleExamples :: String
sampleExamples = unlines
  [ "Examples:"
  , "| username | email              | password      |"
  , "| john doe | john@doe.com       | ABCD1234!?    |"
  , "| jane doe | jane.doe@gmail.com | abcdefgh1.aba |"
  , "| jackson  | jackson@yahoo.com  | cadsw4ll0p/   |"
  ]

producedExample :: ExampleTable
producedExample = ExampleTable
  ["username", "email", "password"]
  [ [("username", ValueString "john doe"), ("email", ValueString "john@doe.com"), ("password", ValueString "ABCD1234!?")]
  , [("username", ValueString "jane doe"), ("email", ValueString "jane.doe@gmail.com"), ("password", ValueString "abcdefgh1.aba")]
  , [("username", ValueString "jackson"), ("email", ValueString "jackson@yahoo.com"), ("password", ValueString "cadsw4ll0p/")]
  ]

statementSpec :: Spec
statementSpec = describe "Parse Statements" $ do
  context "When it's a 'Given' statement with no variables" $
    it "Should parse correctly" $
      shouldMatch parseStatement "Given There is no account with username x" (Statement "There is no account with username x" [])
  context "When it's a 'When' statement with a variable" $
    it "Should parse correctly" $
      shouldMatch parseStatement "When I register with username <username>"
        (Statement "I register with username <username>" ["username"])
  context "When it's a 'Then' statement with two variables" $
    it "Should parse correctly" $
      shouldMatch parseStatement "When I register with username <username> and email <email>"
        (Statement "I register with username <username> and email <email>" ["username", "email"])
  context "When it's an 'And' statement with two variables" $
    it "Should parse correctly" $
      shouldMatch parseStatement "And I register with username <username> and email <email>"
        (Statement "I register with username <username> and email <email>" ["username", "email"])

testFeaturesExtension :: FilePath
testFeaturesExtension = "/test/features/"

regFeatureFile :: FilePath
regFeatureFile = "registration.feature"

loginFeatureFile :: FilePath
loginFeatureFile = "login.feature"

withdrawalFeatureFile :: FilePath
withdrawalFeatureFile = "withdrawal.feature"
