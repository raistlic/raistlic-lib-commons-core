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
    Precondition.param(Boolean.TRUE).isTrue("message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsTrueWithFalseNoMessage() {

    Precondition.param(Boolean.FALSE).isTrue();
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsTrueWithFalseWithMessage() {

    Precondition.param(Boolean.FALSE).isTrue("message");
  }

  @Test
  public void testIsFalseWithFalse() {

    Precondition.param(Boolean.FALSE).isFalse();
    Precondition.param(Boolean.FALSE).isFalse("message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsFalseWithTrueNoMessage() {

    Precondition.param(Boolean.TRUE).isFalse();
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsFalseWithTrueWithMessage() {

    Precondition.param(Boolean.TRUE).isFalse("message");
  }
}
