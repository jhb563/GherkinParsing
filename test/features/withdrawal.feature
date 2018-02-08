Feature: Account Withdrawal

  Background:
    Given There is an existing user with username "test", email "test@test.com" and password "abcd1234!?"

  Scenario: Unauthenticated Withdrawal
    Given user "test" has $200 in their account
    And I am an unauthenticated user
    When I try to withdraw $100 from the user "test"
    Then It should fail with an error: "You aren't logged in"

  Scenario: Successful Withdrawal
    Given user "test" has $500 in their account
    And I am logged in as "test"
    When I try to withdraw $450 from the user "test"
    Then I should receive $450
    And user "test" should have $50 in their account

  Scenario: Overdraw
    Given user "test" has $300 in their account
    And I am logged in as "test"
    When I try to withdraw $400 from the user "test"
    Then It should fail with an error: "You don't have enough money in your account for that withdrawal"
