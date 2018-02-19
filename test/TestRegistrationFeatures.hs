module TestRegistrationFeatures where

import Types
import Utils

registrationFeature :: Feature
registrationFeature = Feature
  { featureTitle = "Registering a user"
  , featureDescription =
      ["As a user"
      , "I want to register"
      , "So that I have an account"
      ]
  , featureBackground = Nothing
  , featureScenarios = 
    [ successfulRegistration
    , emailTaken
    , usernameTaken
    , invalidEmail
    , invalidPassword
    ]
  }

successfulRegistration :: Scenario
successfulRegistration = Scenario
  { scenarioTitle = "Successful Registration"
  , scenarioStatements =
    [ Statement "There is no account with username <username> or email <email>" ["username", "email"]
    , Statement "I register an account with username <username>, email <email> and password <password>" allKeywords
    , Statement "It should successfully create the account with <username>, <email>, and <password>" allKeywords
    ]
  , scenarioExamples = regExamples1
  }

regExamples1 :: ExampleTable
regExamples1 = ExampleTable
  { exampleTableKeys = allKeywords
  , exampleTableExamples =
    [ mkExample "john doe" "john@doe.com" "ABCD1234!?"
    , mkExample "jane doe" "jane.doe@gmail.com" "abcdefgh1.aba"
    , mkExample "jackson" "jackson@yahoo.com" "cadsw4ll0p/"
    ]
  }

emailTaken :: Scenario
emailTaken = Scenario
  { scenarioTitle = "Email Taken"
  , scenarioStatements =
    [ Statement "There is already an account with the email \"test@test.com\"" []
    , Statement "I register an account with username \"test\", email \"test@test.com\" and password \"1234abcd!?\"" []
    , Statement "It should fail with an error: \"An account with that email already exists\"" []
    ]
  , scenarioExamples = ExampleTable [] []
  }

usernameTaken :: Scenario
usernameTaken = Scenario
  { scenarioTitle = "Username Taken"
  , scenarioStatements =
    [ Statement "There is already an account with the username \"test\"" []
    , Statement "I register an account with username \"test\", email \"test@test.com\" and password \"1234abcd!?\"" []
    , Statement "It should fail with an error: \"An account with that username already exists\"" []
    ]
  , scenarioExamples = ExampleTable [] []
  }

invalidEmail :: Scenario
invalidEmail = Scenario
  { scenarioTitle = "Invalid Email"
  , scenarioStatements =
    [ Statement "I register an account with username <username>, email <email> and password <password>" allKeywords
    , Statement "It should fail with an error: \"That email is invalid\"" []
    ]
  , scenarioExamples = regExamples2
  }

regExamples2 :: ExampleTable
regExamples2 = ExampleTable
  { exampleTableKeys = allKeywords
  , exampleTableExamples =
    [ mkExample "john doe" "johndoe" "ABCD1234!?"
    , mkExample "jane doe" "jane@doe" "ABCD1234!?"
    , mkExample "jackson" "jackson.com" "ABCD1234!?"
    ]
  }

invalidPassword :: Scenario
invalidPassword = Scenario
  { scenarioTitle = "Invalid Password"
  , scenarioStatements =
    [ Statement "I register an account with username <username>, email <email> and password <password>" allKeywords
    , Statement "It should fail with an error: \"That password is invalid\"" []
    ]
  , scenarioExamples = regExamples3
  }

regExamples3 :: ExampleTable
regExamples3 = ExampleTable
  { exampleTableKeys = allKeywords
  , exampleTableExamples =
    [ mkExample "john doe" "john@doe.com" "abc.efghijk"
    , mkExample "john doe" "john@doe.com" "!123456789"
    , mkExample "john doe" "john@doe.com" "abCD!/A?las"
    , mkExample "john doe" "john@doe.com" "abcd12?."
    ]
  }
