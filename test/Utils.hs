module Utils where

import Types

mkExample :: String -> String -> String -> [(String, Value)]
mkExample username email password =
  [ ("username", ValueString username)
  , ("email", ValueString email)
  , ("password", ValueString password)
  ]

allKeywords :: [String]
allKeywords = ["username", "email", "password"]
