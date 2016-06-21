package org.raistlic.common.precondition;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

/**
 * @author Lei CHEN (2015-11-23)
 */
@RunWith(JUnit4.class)
public class PreconditionForBooleanBoxedStateTest {

  @Test
  public void testIsTrueWithTrue() {

    Precondition.context(Boolean.TRUE).isTrue();
    Precondition.context(Boolean.TRUE).isTrue("message");
  }

  @Test(expected = InvalidContextException.class)
  public void testIsTrueWithFalseNoMessage() {

    Precondition.context(Boolean.FALSE).isTrue();
  }

  @Test(expected = InvalidContextException.class)
  public void testIsTrueWithFalseWithMessage() {

    Precondition.context(Boolean.FALSE).isTrue("message");
  }

  @Test
  public void testIsFalseWithFalse() {

    Precondition.context(Boolean.FALSE).isFalse();
    Precondition.context(Boolean.FALSE).isFalse("message");
  }

  @Test(expected = InvalidContextException.class)
  public void testIsFalseWithTrueNoMessage() {

    Precondition.context(Boolean.TRUE).isFalse();
  }

  @Test(expected = InvalidContextException.class)
  public void testIsFalseWithTrueWithMessage() {

    Precondition.context(Boolean.TRUE).isFalse("message");
  }
}
