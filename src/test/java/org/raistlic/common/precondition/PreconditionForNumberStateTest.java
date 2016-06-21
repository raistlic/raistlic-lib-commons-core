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

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

/**
 * @author Lei CHEN (2015-11-23)
 */
@RunWith(JUnit4.class)
public class PreconditionForNumberStateTest {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @Test
  public void testEqualToWithEqualStates() {

    int state1 = 123;
    int state2 = 123;

    Precondition.context(state1).isEqualTo(state2);
    Precondition.context(state1).isEqualTo(state2, "message");
  }

  @Test
  public void testEqualToWithNotEqualStatesNoNameNoMessage() {

    int state1 = 123;
    int state2 = 456;

    exception.expect(InvalidContextException.class);
    Precondition.context(state1).isEqualTo(state2);
  }

  @Test
  public void testEqualToWithNotEqualStatesNoNameWithMessage() {

    int state1 = 123;
    int state2 = 456;

    exception.expect(InvalidContextException.class);
    Precondition.context(state1).isEqualTo(state2, "message");
  }

  @Test
  public void testNotEqualToWithNotEqualStates() {

    int state1 = 123;
    int state2 = 456;

    Precondition.context(state1).isNotEqualTo(state2);
    Precondition.context(state1).isNotEqualTo(state2, "message");
  }

  @Test
  public void testNotEqualToWIthEqualStatesNoNameNoMessage() {

    int state1 = 123;
    int state2 = 123;

    exception.expect(InvalidContextException.class);
    Precondition.context(state1).isNotEqualTo(state2);
  }

  @Test
  public void testNotEqualToWIthEqualStatesNoNameWithMessage() {

    int state1 = 123;
    int state2 = 123;

    exception.expect(InvalidContextException.class);
    Precondition.context(state1).isNotEqualTo(state2, "message");
  }

  @Test
  public void testLessThanCheckSucceeds() {

    int state1 = 123;
    int state2 = 124;

    Precondition.context(state1).lessThan(state2);
    Precondition.context(state1).lessThan(state2, "message");
  }

  @Test
  public void testLessThanCheckFailsNoNameNoMessage() {

    int state1 = 123;
    int state2 = 122;

    exception.expect(InvalidContextException.class);
    Precondition.context(state1).lessThan(state2);
  }

  @Test
  public void testLessThanCheckFailsNoNameWithMessage() {

    int state1 = 123;
    int state2 = 122;

    exception.expect(InvalidContextException.class);
    Precondition.context(state1).lessThan(state2, "message");
  }

  @Test
  public void testLessThanWithEqualStatesNoNameNoMessage() {

    exception.expect(InvalidContextException.class);
    Precondition.context(123).lessThan(123);
  }

  @Test
  public void testLessThanWithEqualStatesNoNameWithMessage() {

    exception.expect(InvalidContextException.class);
    Precondition.context(123).lessThan(123, "message");
  }

  @Test
  public void testGreaterThanOrEqualToCheckSucceeds() {

    int state1 = 123;
    int state2 = 122;

    Precondition.context(state1).greaterThanOrEqualTo(state2);
    Precondition.context(state1).greaterThanOrEqualTo(state2, "message");

    state2 = 123;

    Precondition.context(state1).greaterThanOrEqualTo(state2);
    Precondition.context(state1).greaterThanOrEqualTo(state2, "message");
  }

  @Test
  public void testGreaterThanOrEqualToCheckFailsNoNameNoMessage() {

    int state1 = 123;
    int state2 = 124;

    exception.expect(InvalidContextException.class);
    Precondition.context(state1).greaterThanOrEqualTo(state2);
  }

  @Test
  public void testGreaterThanOrEqualToCheckFailsNoNameWithMessage() {

    int state1 = 123;
    int state2 = 124;

    exception.expect(InvalidContextException.class);
    Precondition.context(state1).greaterThanOrEqualTo(state2, "message");
  }

  @Test
  public void testGreaterThanCheckSucceeds() {

    int state1 = 123;
    int state2 = 122;

    Precondition.context(state1).greaterThan(state2);
    Precondition.context(state1).greaterThan(state2, "message");
  }

  @Test
  public void testGreaterThanCheckFailsNoNameNoMessage() {

    int state1 = 123;
    int state2 = 124;

    exception.expect(InvalidContextException.class);
    Precondition.context(state1).greaterThan(state2);
  }

  @Test
  public void testGreaterThanCheckFailsNoNameWithMessage() {

    int state1 = 123;
    int state2 = 124;

    exception.expect(InvalidContextException.class);
    Precondition.context(state1).greaterThan(state2, "message");
  }

  @Test
  public void testGreaterThanWithEqualStatesNoNameNoMessage() {

    exception.expect(InvalidContextException.class);
    Precondition.context(123).greaterThan(123);
  }

  @Test
  public void testGreaterThanWithEqualStatesNoNameWithMessage() {

    exception.expect(InvalidContextException.class);
    Precondition.context(123).greaterThan(123, "message");
  }

  @Test
  public void testLessThanOrEqualToCheckSucceeds() {

    int state1 = 123;
    int state2 = 124;

    Precondition.context(state1).lessThanOrEqualTo(state2);
    Precondition.context(state1).lessThanOrEqualTo(state2, "message");

    state2 = 123;

    Precondition.context(state1).lessThanOrEqualTo(state2);
    Precondition.context(state1).lessThanOrEqualTo(state2, "message");
  }

  @Test
  public void testLessThanOrEqualToCheckFailsNoNameNoMessage() {

    int state1 = 123;
    int state2 = 122;

    exception.expect(InvalidContextException.class);
    Precondition.context(state1).lessThanOrEqualTo(state2);
  }

  @Test
  public void testLessThanOrEqualToCheckFailsNoNameWithMessage() {

    int state1 = 123;
    int state2 = 122;

    exception.expect(InvalidContextException.class);
    Precondition.context(state1).lessThanOrEqualTo(state2, "message");
  }
}
