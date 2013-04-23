Feature: Splice tag

  Scenario: Splice tag
    Given I insert:
    """
    <div>
      <ul>
        <li>abc</li>
        <li>def</li>
      </ul>
    </div>
    """
    When I go to the front of the word "li"
    And I press "M-s"
    Then I should see:
    """
    <div>
      <li>abc</li>
      <li>def</li>
    </div>
    """
    And I press "M-s"
    Then I should see:
    """
    <li>abc</li>
    <li>def</li>
    """
