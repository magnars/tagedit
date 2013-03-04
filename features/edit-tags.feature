Feature: Edit tags

  Background:
    Given I activate tagedit experimental features

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

  Scenario: Edit tag
    Given I insert "<div id="abc">def</div>"
    When I go to the end of the word "div"
    And I type "s"
    Then I should see "<divs id="abc">def</divs>"
