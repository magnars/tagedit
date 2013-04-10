Feature: Splice tag

  Scenario: Raise all children, ditch parent
    Given I insert:
    """
    <div>
      <p>abc</p>
      <span>def</span>
    </div>
    """
    When I go to the front of the word "span"
    And I press "M-s"
    Then I should see:
    """
    <p>abc</p>
    <span>def</span>
    """
