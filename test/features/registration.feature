Feature: Registering a user

  Scenario: Successful Registration
    Given There is no account with username <username> or email <email>
    When I register an account with username <username>, email <email> and password <password>
    Then It should successfully create the account with <username>, <email>, and <password>
    Examples:
      | username | email              | password      |
      | john doe | john@doe.com       | ABCD1234!?    |
      | jane doe | jane.doe@gmail.com | abcdefgh1.aba |
      | jackson  | jackson@yahoo.com  | cadsw4ll0p/   |

  Scenario: Email Taken
    Given There is already an account with the email "test@test.com"
    When I register an account with username "test", email "test@test.com" and password "1234abcd!?"
    Then It should fail with an error: "An account with that email already exists"

  Scenario: Username Taken
    Given There is already an account with the username "test"
    When I register an account with username "test", email "test@test.com" and password "1234abcd!?"
    Then It should fail with an error: "An account with that username already exists"

  Scenario: Invalid Email
    When I register an account with username <username>, email <email> and password <password>
    Then It should fail with an error: "That email is invalid"
    Examples:
      | username | email       | password   |
      | john doe | johndoe     | ABCD1234!? |
      | jane doe | jane@doe    | ABCD1234!? |
      | jackson  | jackson.com | ABCD1234!? |

  Scenario: Invalid Password
    When I register an account with username <username>, email <email> and password <password>
    Then It should fail with an error: "That password is invalid"
    Examples:
      | username | email        | password    |
      | john doe | john@doe.com | abc.efghijk |
      | john doe | john@doe.com | !123456789  |
      | john doe | john@doe.com | abCD!/A?las |
      | john doe | john@doe.com | abcd12?.    |
