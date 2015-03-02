package org.raistlic.common.precondition;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * @author Lei.C (2015-03-02)
 */
public class ParamExpectationOfBooleanTest {

  @Rule
  public ExpectedException thrown = ExpectedException.none();

  @Test
  public void testIsTrueWithTrue() {

    Precondition.param(true).isTrue();
    Precondition.param(true, "name").isTrue();
    Precondition.param(true).isTrue("message");
    Precondition.param(true, "name").isTrue("message");
  }

  @Test
  public void testIsTrueWithFalseNoNameNoMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param(false).isTrue();
  }

  @Test
  public void testIsTrueWithFalseWithNameNoMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param(false, "name").isTrue();
  }

  @Test
  public void testIsTrueWithFalseNoNameWithMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param(false).isTrue("message");
  }

  @Test
  public void testIsTrueWithFalseWithNameWithMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param(false, "name").isTrue("message");
  }

  @Test
  public void testIsFalseWithFalse() {

    Precondition.param(false).isFalse();
    Precondition.param(false, "name").isFalse();
    Precondition.param(false).isFalse("message");
    Precondition.param(false, "name").isFalse("message");
  }

  @Test
  public void testIsFalseWithTrueNoNameNoMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param(true).isFalse();
  }

  @Test
  public void testIsFalseWithTrueWithNameNoMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param(true, "name").isFalse();
  }

  @Test
  public void testIsFalseWithTrueNoNameWithMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param(true).isFalse("message");
  }

  @Test
  public void testIsFalseWithTrueWithNameWithMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param(true, "name").isFalse("message");
  }
}
