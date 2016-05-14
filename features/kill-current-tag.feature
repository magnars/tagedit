Feature: Kill current tag

  Scenario: Kill unpaired tag
    Given I insert "<meta charset=\"utf-8\" /> "
    When I place the cursor after "<"
    And I press "C-c C-<backspace>"
    Then I should see " "

  Scenario: Kill paired tag with content
    Given I insert "<p align=\"left\">The content</p><br />"
    When I place the cursor after "<p"
    And I press "C-c C-<backspace>"
    Then I should see "<br />"
