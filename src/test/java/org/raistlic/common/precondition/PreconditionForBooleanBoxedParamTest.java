package org.raistlic.common.precondition;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

/**
 * @author Lei CHEN (2015-11-23)
 */
@RunWith(JUnit4.class)
public class PreconditionForBooleanBoxedParamTest {

  @Test
  public void testIsTrueWithTrue() {

    Precondition.param(Boolean.TRUE).isTrue();
    Precondition.param(Boolean.TRUE, "name").isTrue();
    Precondition.param(Boolean.TRUE).isTrue("message");
    Precondition.param(Boolean.TRUE, "name").isTrue("message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsTrueWithFalseNoNameNoMessage() {

    Precondition.param(Boolean.FALSE).isTrue();
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsTrueWithFalseWithNameNoMessage() {

    Precondition.param(Boolean.FALSE, "name").isTrue();
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsTrueWithFalseNoNameWithMessage() {

    Precondition.param(Boolean.FALSE).isTrue("message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsTrueWithFalseWithNameWithMessage() {

    Precondition.param(Boolean.FALSE, "name").isTrue("message");
  }

  @Test
  public void testIsFalseWithFalse() {

    Precondition.param(Boolean.FALSE).isFalse();
    Precondition.param(Boolean.FALSE, "name").isFalse();
    Precondition.param(Boolean.FALSE).isFalse("message");
    Precondition.param(Boolean.FALSE, "name").isFalse("message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsFalseWithTrueNoNameNoMessage() {

    Precondition.param(Boolean.TRUE).isFalse();
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsFalseWithTrueWithNameNoMessage() {

    Precondition.param(Boolean.TRUE, "name").isFalse();
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsFalseWithTrueNoNameWithMessage() {

    Precondition.param(Boolean.TRUE).isFalse("message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsFalseWithTrueWithNameWithMessage() {

    Precondition.param(Boolean.TRUE, "name").isFalse("message");
  }
}
