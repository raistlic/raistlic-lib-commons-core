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

    Precondition.state(true).isTrue();
    Precondition.state(true, "name").isTrue();
    Precondition.state(true).isTrue("message");
    Precondition.state(true, "name").isTrue("message");
  }

  @Test(expected = InvalidStateException.class)
  public void testIsTrueWithFalseNoNameNoMessage() {

    Precondition.state(false).isTrue();
  }

  @Test(expected = InvalidStateException.class)
  public void testIsTrueWithFalseWithNameNoMessage() {

    Precondition.state(false, "name").isTrue();
  }

  @Test(expected = InvalidStateException.class)
  public void testIsTrueWithFalseNoNameWithMessage() {

    Precondition.state(false).isTrue("message");
  }

  @Test(expected = InvalidStateException.class)
  public void testIsTrueWithFalseWithNameWithMessage() {

    Precondition.state(false, "name").isTrue("message");
  }

  @Test
  public void testIsFalseWithFalse() {

    Precondition.state(false).isFalse();
    Precondition.state(false, "name").isFalse();
    Precondition.state(false).isFalse("message");
    Precondition.state(false, "name").isFalse("message");
  }

  @Test(expected = InvalidStateException.class)
  public void testIsFalseWithTrueNoNameNoMessage() {

    Precondition.state(true).isFalse();
  }

  @Test(expected = InvalidStateException.class)
  public void testIsFalseWithTrueWithNameNoMessage() {

    Precondition.state(true, "name").isFalse();
  }

  @Test(expected = InvalidStateException.class)
  public void testIsFalseWithTrueNoNameWithMessage() {

    Precondition.state(true).isFalse("message");
  }

  @Test(expected = InvalidStateException.class)
  public void testIsFalseWithTrueWithNameWithMessage() {

    Precondition.state(true, "name").isFalse("message");
  }
}
