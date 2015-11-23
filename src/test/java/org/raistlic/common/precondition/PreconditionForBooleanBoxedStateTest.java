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

    Precondition.state(Boolean.TRUE).isTrue();
    Precondition.state(Boolean.TRUE, "name").isTrue();
    Precondition.state(Boolean.TRUE).isTrue("message");
    Precondition.state(Boolean.TRUE, "name").isTrue("message");
  }

  @Test(expected = InvalidStateException.class)
  public void testIsTrueWithFalseNoNameNoMessage() {

    Precondition.state(Boolean.FALSE).isTrue();
  }

  @Test(expected = InvalidStateException.class)
  public void testIsTrueWithFalseWithNameNoMessage() {

    Precondition.state(Boolean.FALSE, "name").isTrue();
  }

  @Test(expected = InvalidStateException.class)
  public void testIsTrueWithFalseNoNameWithMessage() {

    Precondition.state(Boolean.FALSE).isTrue("message");
  }

  @Test(expected = InvalidStateException.class)
  public void testIsTrueWithFalseWithNameWithMessage() {

    Precondition.state(Boolean.FALSE, "name").isTrue("message");
  }

  @Test
  public void testIsFalseWithFalse() {

    Precondition.state(Boolean.FALSE).isFalse();
    Precondition.state(Boolean.FALSE, "name").isFalse();
    Precondition.state(Boolean.FALSE).isFalse("message");
    Precondition.state(Boolean.FALSE, "name").isFalse("message");
  }

  @Test(expected = InvalidStateException.class)
  public void testIsFalseWithTrueNoNameNoMessage() {

    Precondition.state(Boolean.TRUE).isFalse();
  }

  @Test(expected = InvalidStateException.class)
  public void testIsFalseWithTrueWithNameNoMessage() {

    Precondition.state(Boolean.TRUE, "name").isFalse();
  }

  @Test(expected = InvalidStateException.class)
  public void testIsFalseWithTrueNoNameWithMessage() {

    Precondition.state(Boolean.TRUE).isFalse("message");
  }

  @Test(expected = InvalidStateException.class)
  public void testIsFalseWithTrueWithNameWithMessage() {

    Precondition.state(Boolean.TRUE, "name").isFalse("message");
  }
}
