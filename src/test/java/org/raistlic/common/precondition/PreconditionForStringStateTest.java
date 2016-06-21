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

    Precondition.context((String) null).isNull();
    Precondition.context((String) null).isNull("message");
  }

  @Test(expected = InvalidContextException.class)
  public void testIsNullWithNonNullStateNoNameNoMessage() {

    Precondition.context("abc").isNull();
  }

  @Test(expected = InvalidContextException.class)
  public void testIsNullWithNonNullStateNoNameWithMessage() {

    Precondition.context("abc").isNull("message");
  }

  @Test
  public void testNotNullWithNonNullState() {

    Precondition.context("abc").isNotNull();
    Precondition.context("abc").isNotNull("message");
  }

  @Test(expected = InvalidContextException.class)
  public void testNotNullWithNullStateNoNameNoMessage() {

    Precondition.context((String) null).isNotNull();
  }

  @Test(expected = InvalidContextException.class)
  public void testNotNullWithNullStateNoNameWithMessage() {

    Precondition.context((String) null).isNotNull("message");
  }

  @Test
  public void testEqualToWithEqualStates() {

    String state1 = "abc";
    String state2 = "abc";

    Precondition.context(state1).isEqualTo(state2);
    Precondition.context(state1).isEqualTo(state2, "message");
  }

  @Test(expected = InvalidContextException.class)
  public void testEqualToWithNotEqualStatesNoNameNoMessage() {

    String state1 = "abc";
    String state2 = "def";

    Precondition.context(state1).isEqualTo(state2);
  }

  @Test(expected = InvalidContextException.class)
  public void testEqualToWithNotEqualStatesNoNameWithMessage() {

    String state1 = "abc";
    String state2 = "def";

    Precondition.context(state1).isEqualTo(state2, "message");
  }

  @Test
  public void testNotEqualToWithNotEqualStates() {

    String state1 = "abc";
    String state2 = "def";

    Precondition.context(state1).isNotEqualTo(state2);
    Precondition.context(state1).isNotEqualTo(state2, "message");
  }

  @Test(expected = InvalidContextException.class)
  public void testNotEqualToWithEqualStatesNoNameNoMessage() {

    String state1 = "abc";
    String state2 = "abc";

    Precondition.context(state1).isNotEqualTo(state2);
  }

  @Test(expected = InvalidContextException.class)
  public void testNotEqualToWithEqualStatesNoNameWithMessage() {

    String state1 = "abc";
    String state2 = "abc";

    Precondition.context(state1).isNotEqualTo(state2, "message");
  }

  @Test
  public void testIsEmptyWithEmptyString() {

    Precondition.context("").isEmpty();
    Precondition.context("").isEmpty("message");
  }

  @Test(expected = InvalidContextException.class)
  public void testIsEmptyWithNonEmptyStringNoNameNoMessage() {

    Precondition.context("abc").isEmpty();
  }

  @Test(expected = InvalidContextException.class)
  public void testIsEmptyWithNonEmptyStringNoNameWithMessage() {

    Precondition.context("abc").isEmpty("message");
  }

  @Test
  public void testNotEmptyWithNonEmptyState() {

    Precondition.context("abc").isNotEmpty();
    Precondition.context("abc").isNotEmpty("message");
  }

  @Test(expected = InvalidContextException.class)
  public void testNotEmptyWithEmptyStateNoNameNoMessage() {

    Precondition.context("").isNotEmpty();
  }

  @Test(expected = InvalidContextException.class)
  public void testNotEmptyWithEmptyStateNoNameWithMessage() {

    Precondition.context("").isNotEmpty("message");
  }
}
