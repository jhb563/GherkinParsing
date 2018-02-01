module TestWithdrawalFeatures where

import Types
import Utils

withdrawalFeature :: Feature
withdrawalFeature = Feature
  { featureTitle = "Account Withdrawal"
  , featureDescription =
    [ "The user should be able to withdraw an amount of money up to their account balance"
    , "The user should not be able to withdraw more than their balance"
    , "Users must be logged in in order to withdraw"
    ]
  , featureBackground = Just backgroundScenario
  , featureScenarios = 
    [ unauthedWithdrawal
    , successfulWithdrawal
    , overdraw
    ]
  }

backgroundScenario :: Scenario
backgroundScenario = Scenario
  { scenarioTitle = "Background"
  , scenarioStatements =
    [ Statement "There is an existing user with username \"test\", email \"test@test.com\" and password \"abcd1234!?\"" []
    ]
  , scenarioExamples = ExampleTable [] []
  }

unauthedWithdrawal :: Scenario
unauthedWithdrawal = Scenario
  { scenarioTitle = "Unauthenticated Withdrawal"
  , scenarioStatements =
    [ Statement "user \"test\" has $200 in their account" []
    , Statement "I am an unauthenticated user" []
    , Statement "I try to withdraw $100 from the user \"test\"" []
    , Statement "it should fail with an error: \"You aren't logged in\"" []
    ]
  , scenarioExamples = ExampleTable [] []
  }

successfulWithdrawal :: Scenario
successfulWithdrawal = Scenario
  { scenarioTitle = "Successful Withdrawal"
  , scenarioStatements =
    [ Statement "user \"test\" has $500 in their account" []
    , Statement "I am logged in as \"test\"" []
    , Statement "I try to withdraw $450 from the user \"test\"" []
    , Statement "I should receive $450" []
    , Statement "user \"test\" should have $50 in their account" []
    ]
  , scenarioExamples = ExampleTable [] []
  }

overdraw :: Scenario
overdraw = Scenario
  { scenarioTitle = "Overdraw"
  , scenarioStatements =
    [ Statement "user \"test\" has $300 in their account" []
    , Statement "I am logged in as \"test\"" []
    , Statement "I try to withdraw $400 from the user \"test\"" []
    , Statement "it should fail with an error: \"You don't have enough money in your account for that withdrawal\"" []
    ]
  , scenarioExamples = ExampleTable [] []
  }
