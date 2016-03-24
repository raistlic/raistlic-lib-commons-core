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
public class PreconditionForStringStateTest {

  @Test
  public void testIsNullWithNullState() {

    Precondition.state((String) null).isNull();
    Precondition.state((String) null, "name").isNull();
    Precondition.state((String) null).isNull("message");
    Precondition.state((String) null, "name").isNull("message");
  }

  @Test(expected = InvalidStateException.class)
  public void testIsNullWithNonNullStateNoNameNoMessage() {

    Precondition.state("abc").isNull();
  }

  @Test(expected = InvalidStateException.class)
  public void testIsNullWithNonNullStateWithNameNoMessage() {

    Precondition.state("abc", "name").isNull();
  }

  @Test(expected = InvalidStateException.class)
  public void testIsNullWithNonNullStateNoNameWithMessage() {

    Precondition.state("abc").isNull("message");
  }

  @Test(expected = InvalidStateException.class)
  public void testIsNullWithNonNullStateWithNameWithMessage() {

    Precondition.state("abc", "name").isNull("message");
  }

  @Test
  public void testNotNullWithNonNullState() {

    Precondition.state("abc").isNotNull();
    Precondition.state("abc", "name").isNotNull();
    Precondition.state("abc").isNotNull("message");
    Precondition.state("abc", "name").isNotNull("message");
  }

  @Test(expected = InvalidStateException.class)
  public void testNotNullWithNullStateNoNameNoMessage() {

    Precondition.state((String) null).isNotNull();
  }

  @Test(expected = InvalidStateException.class)
  public void testNotNullWithNullStateWithNameNoMessage() {

    Precondition.state((String) null, "name").isNotNull();
  }

  @Test(expected = InvalidStateException.class)
  public void testNotNullWithNullStateNoNameWithMessage() {

    Precondition.state((String) null).isNotNull("message");
  }

  @Test(expected = InvalidStateException.class)
  public void testNotNullWithNullStateWithNameWithMessage() {

    Precondition.state((String) null, "name").isNotNull("message");
  }

  @Test
  public void testEqualToWithEqualStates() {

    String state1 = "abc";
    String state2 = "abc";

    Precondition.state(state1).isEqualTo(state2);
    Precondition.state(state1, "name").isEqualTo(state2);
    Precondition.state(state1).isEqualTo(state2, "message");
    Precondition.state(state1, "name").isEqualTo(state2, "message");
  }

  @Test(expected = InvalidStateException.class)
  public void testEqualToWithNotEqualStatesNoNameNoMessage() {

    String state1 = "abc";
    String state2 = "def";

    Precondition.state(state1).isEqualTo(state2);
  }

  @Test(expected = InvalidStateException.class)
  public void testEqualToWithNotEqualStatesWithNameNoMessage() {

    String state1 = "abc";
    String state2 = "def";

    Precondition.state(state1, "name").isEqualTo(state2);
  }

  @Test(expected = InvalidStateException.class)
  public void testEqualToWithNotEqualStatesNoNameWithMessage() {

    String state1 = "abc";
    String state2 = "def";

    Precondition.state(state1).isEqualTo(state2, "message");
  }

  @Test(expected = InvalidStateException.class)
  public void testEqualToWithNotEqualStatesWithNameWithMessage() {

    String state1 = "abc";
    String state2 = "def";

    Precondition.state(state1, "name").isEqualTo(state2, "message");
  }

  @Test
  public void testNotEqualToWithNotEqualStates() {

    String state1 = "abc";
    String state2 = "def";

    Precondition.state(state1).isNotEqualTo(state2);
    Precondition.state(state1, "name").isNotEqualTo(state2);
    Precondition.state(state1).isNotEqualTo(state2, "message");
    Precondition.state(state1, "name").isNotEqualTo(state2, "message");
  }

  @Test(expected = InvalidStateException.class)
  public void testNotEqualToWithEqualStatesNoNameNoMessage() {

    String state1 = "abc";
    String state2 = "abc";

    Precondition.state(state1).isNotEqualTo(state2);
  }

  @Test(expected = InvalidStateException.class)
  public void testNotEqualToWithEqualStatesWithNameNoMessage() {

    String state1 = "abc";
    String state2 = "abc";

    Precondition.state(state1, "name").isNotEqualTo(state2);
  }

  @Test(expected = InvalidStateException.class)
  public void testNotEqualToWithEqualStatesNoNameWithMessage() {

    String state1 = "abc";
    String state2 = "abc";

    Precondition.state(state1).isNotEqualTo(state2, "message");
  }

  @Test(expected = InvalidStateException.class)
  public void testNotEqualToWithEqualStatesWithNameWithMessage() {

    String state1 = "abc";
    String state2 = "abc";

    Precondition.state(state1, "name").isNotEqualTo(state2, "message");
  }

  @Test
  public void testIsEmptyWithEmptyString() {

    Precondition.state("").isEmpty();
    Precondition.state("", "name").isEmpty();
    Precondition.state("").isEmpty("message");
    Precondition.state("", "name").isEmpty("message");
  }

  @Test(expected = InvalidStateException.class)
  public void testIsEmptyWithNonEmptyStringNoNameNoMessage() {

    Precondition.state("abc").isEmpty();
  }

  @Test(expected = InvalidStateException.class)
  public void testIsEmptyWithNonEmptyStringWithNameNoMessage() {

    Precondition.state("abc", "name").isEmpty();
  }

  @Test(expected = InvalidStateException.class)
  public void testIsEmptyWithNonEmptyStringNoNameWithMessage() {

    Precondition.state("abc").isEmpty("message");
  }

  @Test(expected = InvalidStateException.class)
  public void testIsEmptyWithNonEmptyStringWithNameWithMessage() {

    Precondition.state("abc", "name").isEmpty("message");
  }

  @Test
  public void testNotEmptyWithNonEmptyState() {

    Precondition.state("abc").notEmpty();
    Precondition.state("abc", "name").notEmpty();
    Precondition.state("abc").notEmpty("message");
    Precondition.state("abc", "name").notEmpty("message");
  }

  @Test(expected = InvalidStateException.class)
  public void testNotEmptyWithEmptyStateNoNameNoMessage() {

    Precondition.state("").notEmpty();
  }

  @Test(expected = InvalidStateException.class)
  public void testNotEmptyWithEmptyStateWithNameNoMessage() {

    Precondition.state("", "name").notEmpty();
  }

  @Test(expected = InvalidStateException.class)
  public void testNotEmptyWithEmptyStateNoNameWithMessage() {

    Precondition.state("").notEmpty("message");
  }

  @Test(expected = InvalidStateException.class)
  public void testNotEmptyWithEmptyStateWithNameWithMessage() {

    Precondition.state("", "name").notEmpty("message");
  }
}
