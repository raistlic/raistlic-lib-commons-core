package org.raistlic.common.precondition;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

/**
 * @author Lei CHEN (2015-11-23)
 */
@RunWith(JUnit4.class)
public class PreconditionForBooleanPrimitiveStateTest {

  @Test
  public void testIsTrueWithTrue() {

    Precondition.context(true).isTrue();
    Precondition.context(true).isTrue("message");
  }

  @Test(expected = InvalidContextException.class)
  public void testIsTrueWithFalseNoMessage() {

    Precondition.context(false).isTrue();
  }

  @Test(expected = InvalidContextException.class)
  public void testIsTrueWithFalseWithMessage() {

    Precondition.context(false).isTrue("message");
  }

  @Test
  public void testIsFalseWithFalse() {

    Precondition.context(false).isFalse();
    Precondition.context(false).isFalse("message");
  }

  @Test(expected = InvalidContextException.class)
  public void testIsFalseWithTrueNoMessage() {

    Precondition.context(true).isFalse();
  }

  @Test(expected = InvalidContextException.class)
  public void testIsFalseWithTrueWithMessage() {

    Precondition.context(true).isFalse("message");
  }
}
