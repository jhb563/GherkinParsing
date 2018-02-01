module Types where

import Data.Scientific (Scientific)

data Feature = Feature
  { featureTitle :: String
  , featureDescription :: [String]
  , featureBackground :: Maybe Scenario
  , featureScenarios :: [Scenario]
  }
  deriving (Show, Eq)

data Scenario = Scenario
  { scenarioTitle :: String
  , scenarioStatements :: [Statement]
  , scenarioExamples :: ExampleTable
  }
  deriving (Show, Eq)

data Statement = Statement
  { statementRegex :: String
  , statementExampleVariables :: [String]
  }
  deriving (Show, Eq)

data ExampleTable = ExampleTable
  { exampleTableKeys :: [String]
  , exampleTableExamples :: [[(String, Value)]]
  }
  deriving (Show, Eq)

data Value =
  ValueNumber Scientific |
  ValueString String |
  ValueBool Bool |
  ValueNull
  deriving (Show, Eq)
