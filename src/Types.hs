module Types where

import qualified Data.Map as Map
import           Data.Scientific (Scientific)

data Feature = Feature
  { featureTitle :: String
  , featureDescription :: [String]
  , featureBackground :: ExampleTable
  , featureScenarios :: [Scenario]
  }

data Scenario = Scenario
  { scenarioTitle :: String
  , scenarioStatements :: Statement
  , scenarioExamples :: ExampleTable
  }

data Statement = Statement
  { statementRegex :: String
  , statementExampleVariables :: [String]
  }

data ExampleTable = ExampleTable
  { exampleTableKeys :: [String]
  , exampleTableExamples :: Map.Map String Value
  }

data Value =
  ValueNumber Scientific |
  ValueString String |
  ValueBool Bool |
  ValueNull
