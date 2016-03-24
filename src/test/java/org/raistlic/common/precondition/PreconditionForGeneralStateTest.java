/*
 * Copyright 2015 Lei CHEN (raistlic@gmail.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.raistlic.common.precondition;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

/**
 * @author Lei CHEN (2015-11-23)
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

    Precondition.state(new Object()).isNotNull();
    Precondition.state(new Object()).isNotNull("message");
    Precondition.state(new Object(), "name").isNotNull();
    Precondition.state(new Object(), "name").isNotNull("message");
  }

  @Test(expected = InvalidStateException.class)
  public void testNotNullWithNullStateNoNameNoMessage() {

    Precondition.state((Object) null).isNotNull();
  }

  @Test(expected = InvalidStateException.class)
  public void testNotNullWithNullStateWithNameNoMessage() {

    Precondition.state((Object) null, "name").isNotNull();
  }

  @Test(expected = InvalidStateException.class)
  public void testNotNullWithNullStateNoNameWithMessage() {

    Precondition.state((Object) null).isNotNull("message");
  }

  @Test(expected = InvalidStateException.class)
  public void testNotNullWithNullStateWithNameWithMessage() {

    Precondition.state((Object) null, "name").isNotNull("message");
  }

  @Test
  public void testEqualToWithEqualStates() {

    Object state1 = new Object();

    Precondition.state(state1).isEqualTo(state1);
    Precondition.state(state1, "name").isEqualTo(state1);
    Precondition.state(state1).isEqualTo(state1, "message");
    Precondition.state(state1, "name").isEqualTo(state1, "message");
  }

  @Test(expected = InvalidStateException.class)
  public void testEqualToWithNotEqualStatesNoNameNoMessage() {

    Object state1 = new Object();
    Object state2 = new Object();

    Precondition.state(state1).isEqualTo(state2);
  }

  @Test(expected = InvalidStateException.class)
  public void testEqualToWithNotEqualStatesWithNameNoMessage() {

    Object state1 = new Object();
    Object state2 = new Object();

    Precondition.state(state1, "name").isEqualTo(state2);
  }

  @Test(expected = InvalidStateException.class)
  public void testEqualToWithNotEqualStatesNoNameWithMessage() {

    Object state1 = new Object();
    Object state2 = new Object();

    Precondition.state(state1).isEqualTo(state2, "message");
  }

  @Test(expected = InvalidStateException.class)
  public void testEqualToWithNotEqualStatesWithNameWithMessage() {

    Object state1 = new Object();
    Object state2 = new Object();

    Precondition.state(state1, "name").isEqualTo(state2, "message");
  }

  @Test
  public void testNotEqualToWithNotEqualStates() {

    Object state1 = new Object();
    Object state2 = new Object();

    Precondition.state(state1).isNotEqualTo(state2);
    Precondition.state(state1, "name").isNotEqualTo(state2);
    Precondition.state(state1).isNotEqualTo(state2, "message");
    Precondition.state(state1, "name").isNotEqualTo(state2, "message");
  }

  @Test(expected = InvalidStateException.class)
  public void testNotEqualToWithEqualStatesNoNameNoMessage() {

    Object state1 = new Object();

    Precondition.state(state1).isNotEqualTo(state1);
  }

  @Test(expected = InvalidStateException.class)
  public void testNotEqualToWithEqualStatesWithNameNoMessage() {

    Object state1 = new Object();

    Precondition.state(state1, "name").isNotEqualTo(state1);
  }

  @Test(expected = InvalidStateException.class)
  public void testNotEqualToWithEqualStatesNoNameWithMessage() {

    Object state1 = new Object();

    Precondition.state(state1).isNotEqualTo(state1, "message");
  }

  @Test(expected = InvalidStateException.class)
  public void testNotEqualToWithEqualStatesWithNameWithMessage() {

    Object state1 = new Object();

    Precondition.state(state1, "name").isNotEqualTo(state1, "message");
  }
}
