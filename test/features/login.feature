Feature: User Log In

  Background:
    Given There is an existing user with username "test", email "test@test.com" and password "abcd1234!?"

  Scenario: Email Log In
    When the user logs in with email "test@test.com" and password "abcd1234!?"
    Then the user should be successfully logged in as "test"

  Scenario: Username Log In
    When the user logs in with username "test" and password "abcd1234!?"
    Then the user should be successfully logged in as "test"

  Scenario: Wrong Email
    When the user logs in with email "test2@test.com" and password "abcd1234!?"
    Then the login attempt should fail with error: "We couldn't find that account information"

  Scenario: Wrong Username
    When the user logs in with username "test2" and password "abcd1234!?"
    Then the login attempt should fail with error: "We couldn't find that account information"

  Scenario: Wrong Password
    When the user logs in with email "test@test.com" and password "ABCD4321@#"
    Then the login attempt should fail with error: "We couldn't find that account information"
