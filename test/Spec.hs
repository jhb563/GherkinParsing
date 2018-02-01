module Main where

import System.Directory
import Test.Hspec

import Parser
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

testFeaturesExtension :: FilePath
testFeaturesExtension = "/test/features/"

regFeatureFile :: FilePath
regFeatureFile = "registration.feature"

loginFeatureFile :: FilePath
loginFeatureFile = "login.feature"

withdrawalFeatureFile :: FilePath
withdrawalFeatureFile = "withdrawal.feature"
