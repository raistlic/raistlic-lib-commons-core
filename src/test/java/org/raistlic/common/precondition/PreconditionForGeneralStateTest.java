package org.raistlic.common.precondition;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

/**
 * @author lei.c (2015-11-23)
 */
@RunWith(JUnit4.class)
public class PreconditionForGeneralStateTest {

  @Test
  public void testIsNullWithNullState() {

    Precondition.state((Object) null).isNull();
    Precondition.state((Object) null, "name").isNull();
    Precondition.state((Object) null).isNull("message");
    Precondition.state((Object) null, "name").isNull("message");
  }

  @Test(expected = InvalidStateException.class)
  public void testIsNullWithNonNullStateNoNameNoMessage() {

    Precondition.state(new Object()).isNull();
  }

  @Test(expected = InvalidStateException.class)
  public void testIsNullWithNonNullStateWithNameNoMessage() {

    Precondition.state(new Object(), "name").isNull();
  }

  @Test(expected = InvalidStateException.class)
  public void testIsNullWithNonNullStateNoNameWithMessage() {

    Precondition.state(new Object()).isNull("message");
  }

  @Test(expected = InvalidStateException.class)
  public void testIsNullWithNonNullStateWithNameWithMessage() {

    Precondition.state(new Object(), "name").isNull("message");
  }

  @Test
  public void testNotNullWithNonNullState() {

    Precondition.state(new Object()).notNull();
    Precondition.state(new Object()).notNull("message");
    Precondition.state(new Object(), "name").notNull();
    Precondition.state(new Object(), "name").notNull("message");
  }

  @Test(expected = InvalidStateException.class)
  public void testNotNullWithNullStateNoNameNoMessage() {

    Precondition.state((Object) null).notNull();
  }

  @Test(expected = InvalidStateException.class)
  public void testNotNullWithNullStateWithNameNoMessage() {

    Precondition.state((Object) null, "name").notNull();
  }

  @Test(expected = InvalidStateException.class)
  public void testNotNullWithNullStateNoNameWithMessage() {

    Precondition.state((Object) null).notNull("message");
  }

  @Test(expected = InvalidStateException.class)
  public void testNotNullWithNullStateWithNameWithMessage() {

    Precondition.state((Object) null, "name").notNull("message");
  }

  @Test
  public void testEqualToWithEqualStates() {

    Object state1 = new Object();

    Precondition.state(state1).equalTo(state1);
    Precondition.state(state1, "name").equalTo(state1);
    Precondition.state(state1).equalTo(state1, "message");
    Precondition.state(state1, "name").equalTo(state1, "message");
  }

  @Test(expected = InvalidStateException.class)
  public void testEqualToWithNotEqualStatesNoNameNoMessage() {

    Object state1 = new Object();
    Object state2 = new Object();

    Precondition.state(state1).equalTo(state2);
  }

  @Test(expected = InvalidStateException.class)
  public void testEqualToWithNotEqualStatesWithNameNoMessage() {

    Object state1 = new Object();
    Object state2 = new Object();

    Precondition.state(state1, "name").equalTo(state2);
  }

  @Test(expected = InvalidStateException.class)
  public void testEqualToWithNotEqualStatesNoNameWithMessage() {

    Object state1 = new Object();
    Object state2 = new Object();

    Precondition.state(state1).equalTo(state2, "message");
  }

  @Test(expected = InvalidStateException.class)
  public void testEqualToWithNotEqualStatesWithNameWithMessage() {

    Object state1 = new Object();
    Object state2 = new Object();

    Precondition.state(state1, "name").equalTo(state2, "message");
  }

  @Test
  public void testNotEqualToWithNotEqualStates() {

    Object state1 = new Object();
    Object state2 = new Object();

    Precondition.state(state1).notEqualTo(state2);
    Precondition.state(state1, "name").notEqualTo(state2);
    Precondition.state(state1).notEqualTo(state2, "message");
    Precondition.state(state1, "name").notEqualTo(state2, "message");
  }

  @Test(expected = InvalidStateException.class)
  public void testNotEqualToWithEqualStatesNoNameNoMessage() {

    Object state1 = new Object();

    Precondition.state(state1).notEqualTo(state1);
  }

  @Test(expected = InvalidStateException.class)
  public void testNotEqualToWithEqualStatesWithNameNoMessage() {

    Object state1 = new Object();

    Precondition.state(state1, "name").notEqualTo(state1);
  }

  @Test(expected = InvalidStateException.class)
  public void testNotEqualToWithEqualStatesNoNameWithMessage() {

    Object state1 = new Object();

    Precondition.state(state1).notEqualTo(state1, "message");
  }

  @Test(expected = InvalidStateException.class)
  public void testNotEqualToWithEqualStatesWithNameWithMessage() {

    Object state1 = new Object();

    Precondition.state(state1, "name").notEqualTo(state1, "message");
  }
}
