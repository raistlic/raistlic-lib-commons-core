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

    Precondition.context((Object) null).isNull();
    Precondition.context((Object) null).isNull("message");
  }

  @Test(expected = InvalidContextException.class)
  public void testIsNullWithNonNullStateNoNameNoMessage() {

    Precondition.context(new Object()).isNull();
  }

  @Test(expected = InvalidContextException.class)
  public void testIsNullWithNonNullStateNoNameWithMessage() {

    Precondition.context(new Object()).isNull("message");
  }

  @Test
  public void testNotNullWithNonNullState() {

    Precondition.context(new Object()).isNotNull();
    Precondition.context(new Object()).isNotNull("message");
  }

  @Test(expected = InvalidContextException.class)
  public void testNotNullWithNullStateNoNameNoMessage() {

    Precondition.context((Object) null).isNotNull();
  }

  @Test(expected = InvalidContextException.class)
  public void testNotNullWithNullStateNoNameWithMessage() {

    Precondition.context((Object) null).isNotNull("message");
  }

  @Test
  public void testEqualToWithEqualStates() {

    Object state1 = new Object();

    Precondition.context(state1).isEqualTo(state1);
    Precondition.context(state1).isEqualTo(state1, "message");
  }

  @Test(expected = InvalidContextException.class)
  public void testEqualToWithNotEqualStatesNoNameNoMessage() {

    Object state1 = new Object();
    Object state2 = new Object();

    Precondition.context(state1).isEqualTo(state2);
  }

  @Test(expected = InvalidContextException.class)
  public void testEqualToWithNotEqualStatesNoNameWithMessage() {

    Object state1 = new Object();
    Object state2 = new Object();

    Precondition.context(state1).isEqualTo(state2, "message");
  }

  @Test
  public void testNotEqualToWithNotEqualStates() {

    Object state1 = new Object();
    Object state2 = new Object();

    Precondition.context(state1).isNotEqualTo(state2);
    Precondition.context(state1).isNotEqualTo(state2, "message");
  }

  @Test(expected = InvalidContextException.class)
  public void testNotEqualToWithEqualStatesNoNameNoMessage() {

    Object state1 = new Object();

    Precondition.context(state1).isNotEqualTo(state1);
  }

  @Test(expected = InvalidContextException.class)
  public void testNotEqualToWithEqualStatesNoNameWithMessage() {

    Object state1 = new Object();

    Precondition.context(state1).isNotEqualTo(state1, "message");
  }
}
