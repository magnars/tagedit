Feature: Misc

  Scenario: Insert quotes when writing attribute (but not otherwise)
    Given I insert "<div>abc</div>"
    When I go to the end of the word "div"
    And I type " id=d=f"
    And I press "C-3 C-f"
    And I type "="
    Then I should see "<div id="d=f">a=bc</div>"
