module TestLoginFeatures where

import Types
import Utils

loginFeature :: Feature
loginFeature = Feature
  { featureTitle = "User Log In"
  , featureDescription = []
  , featureBackground = Just backgroundScenario
  , featureScenarios = 
    [ emailLogin
    , usernameLogin
    , wrongEmail
    , wrongUsername
    , wrongPassword
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

emailLogin :: Scenario
emailLogin = Scenario
  { scenarioTitle = "Email Log In"
  , scenarioStatements =
    [ Statement "the user logs in with email \"test@test.com\" and password \"abcd1234!?\"" []
    , Statement "the user should be successfully logged in as \"test\"" []
    ]
  , scenarioExamples = ExampleTable [] []
  }

usernameLogin :: Scenario
usernameLogin = Scenario
  { scenarioTitle = "Username Log In"
  , scenarioStatements =
    [ Statement "the user logs in with username \"test\" and password \"abcd1234!?\"" []
    , Statement "the user should be successfully logged in as \"test\"" []
    ]
  , scenarioExamples = ExampleTable [] []
  }

wrongEmail :: Scenario
wrongEmail = Scenario
  { scenarioTitle = "Wrong Email"
  , scenarioStatements =
    [ Statement "the user logs in with email \"test2@test.com\" and password \"abcd1234!?\"" []
    , Statement "the login attempt should fail with error: \"We couldn't find that account information\"" []
    ]
  , scenarioExamples = ExampleTable [] []
  }

wrongUsername :: Scenario
wrongUsername = Scenario
  { scenarioTitle = "Wrong Username"
  , scenarioStatements =
    [ Statement "the user logs in with username \"test2\" and password \"abcd1234!?\"" []
    , Statement "the login attempt should fail with error: \"We couldn't find that account information\"" []
    ]
  , scenarioExamples = ExampleTable [] []
  }

wrongPassword :: Scenario
wrongPassword = Scenario
  { scenarioTitle = "Wrong Password"
  , scenarioStatements =
    [ Statement "the user logs in with email \"test@test.com\" and password \"ABCD4321@#\"" []
    , Statement "the login attempt should fail with error: \"We couldn't find that account information\"" []
    ]
  , scenarioExamples = ExampleTable [] []
  }
