Feature: Edit tags

  Scenario: Insert tag
    When I type "<div"
    Then I should see "<div></div>"

  Scenario: Insert tag with attribute
    When I type "<div data-something"
    Then I should see "<div data-something></div>"

  Scenario: Insert self-closing tag
    When I type "<input type"
    Then I should see "<input type>"
    And I should not see "</input>"
