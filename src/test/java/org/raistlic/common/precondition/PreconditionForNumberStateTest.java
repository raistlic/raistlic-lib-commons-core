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

    Precondition.state(state1).isEqualTo(state2);
    Precondition.state(state1, "name").isEqualTo(state2);
    Precondition.state(state1).isEqualTo(state2, "message");
    Precondition.state(state1, "name").isEqualTo(state2, "message");
  }

  @Test
  public void testEqualToWithNotEqualStatesNoNameNoMessage() {

    int state1 = 123;
    int state2 = 456;

    exception.expect(InvalidStateException.class);
    Precondition.state(state1).isEqualTo(state2);
  }

  @Test
  public void testEqualToWithNotEqualStatesWithNameNoMessage() {

    int state1 = 123;
    int state2 = 456;

    exception.expect(InvalidStateException.class);
    Precondition.state(state1, "name").isEqualTo(state2);
  }

  @Test
  public void testEqualToWithNotEqualStatesNoNameWithMessage() {

    int state1 = 123;
    int state2 = 456;

    exception.expect(InvalidStateException.class);
    Precondition.state(state1).isEqualTo(state2, "message");
  }

  @Test
  public void testEqualToWithNotEqualStatesWithNameWithMessage() {

    int state1 = 123;
    int state2 = 456;

    exception.expect(InvalidStateException.class);
    Precondition.state(state1, "name").isEqualTo(state2, "message");
  }

  @Test
  public void testNotEqualToWithNotEqualStates() {

    int state1 = 123;
    int state2 = 456;

    Precondition.state(state1).isNotEqualTo(state2);
    Precondition.state(state1, "name").isNotEqualTo(state2);
    Precondition.state(state1).isNotEqualTo(state2, "message");
    Precondition.state(state1, "name").isNotEqualTo(state2, "message");
  }

  @Test
  public void testNotEqualToWIthEqualStatesNoNameNoMessage() {

    int state1 = 123;
    int state2 = 123;

    exception.expect(InvalidStateException.class);
    Precondition.state(state1).isNotEqualTo(state2);
  }

  @Test
  public void testNotEqualToWIthEqualStatesWithNameNoMessage() {

    int state1 = 123;
    int state2 = 123;

    exception.expect(InvalidStateException.class);
    Precondition.state(state1, "name").isNotEqualTo(state2);
  }

  @Test
  public void testNotEqualToWIthEqualStatesNoNameWithMessage() {

    int state1 = 123;
    int state2 = 123;

    exception.expect(InvalidStateException.class);
    Precondition.state(state1).isNotEqualTo(state2, "message");
  }

  @Test
  public void testNotEqualToWithEqualStatesWithNameWithMessage() {

    int state1 = 123;
    int state2 = 123;

    exception.expect(InvalidStateException.class);
    Precondition.state(state1, "name").isNotEqualTo(state2, "message");
  }

  @Test
  public void testLessThanCheckSucceeds() {

    int state1 = 123;
    int state2 = 124;

    Precondition.state(state1).lessThan(state2);
    Precondition.state(state1, "name").lessThan(state2);
    Precondition.state(state1).lessThan(state2, "message");
    Precondition.state(state1, "name").lessThan(state2, "message");
  }

  @Test
  public void testLessThanCheckFailsNoNameNoMessage() {

    int state1 = 123;
    int state2 = 122;

    exception.expect(InvalidStateException.class);
    Precondition.state(state1).lessThan(state2);
  }

  @Test
  public void testLessThanCheckFailsWithNameNoMessage() {

    int state1 = 123;
    int state2 = 122;

    exception.expect(InvalidStateException.class);
    Precondition.state(state1, "name").lessThan(state2);
  }

  @Test
  public void testLessThanCheckFailsNoNameWithMessage() {

    int state1 = 123;
    int state2 = 122;

    exception.expect(InvalidStateException.class);
    Precondition.state(state1).lessThan(state2, "message");
  }

  @Test
  public void testLessThanCheckFailsWithNameWithMessage() {

    int state1 = 123;
    int state2 = 122;

    exception.expect(InvalidStateException.class);
    Precondition.state(state1, "name").lessThan(state2, "message");
  }

  @Test
  public void testLessThanWithEqualStatesNoNameNoMessage() {

    exception.expect(InvalidStateException.class);
    Precondition.state(123).lessThan(123);
  }

  @Test
  public void testLessThanWithEqualStatesWithNameNoMessage() {

    exception.expect(InvalidStateException.class);
    Precondition.state(123, "name").lessThan(123);
  }

  @Test
  public void testLessThanWithEqualStatesNoNameWithMessage() {

    exception.expect(InvalidStateException.class);
    Precondition.state(123).lessThan(123, "message");
  }

  @Test
  public void testLessThanWithEqualStatesWithNameWithMessage() {

    exception.expect(InvalidStateException.class);
    Precondition.state(123, "name").lessThan(123, "message");
  }

  @Test
  public void testNoLessThanCheckSucceeds() {

    int state1 = 123;
    int state2 = 122;

    Precondition.state(state1).noLessThan(state2);
    Precondition.state(state1, "name").noLessThan(state2);
    Precondition.state(state1).noLessThan(state2, "message");
    Precondition.state(state1, "name").noLessThan(state2, "message");

    state2 = 123;

    Precondition.state(state1).noLessThan(state2);
    Precondition.state(state1, "name").noLessThan(state2);
    Precondition.state(state1).noLessThan(state2, "message");
    Precondition.state(state1, "name").noLessThan(state2, "message");
  }

  @Test
  public void testNoLessThanCheckFailsNoNameNoMessage() {

    int state1 = 123;
    int state2 = 124;

    exception.expect(InvalidStateException.class);
    Precondition.state(state1).noLessThan(state2);
  }

  @Test
  public void testNoLessThanCheckFailsWithNameNoMessage() {

    int state1 = 123;
    int state2 = 124;

    exception.expect(InvalidStateException.class);
    Precondition.state(state1, "name").noLessThan(state2);
  }

  @Test
  public void testNoLessThanCheckFailsNoNameWithMessage() {

    int state1 = 123;
    int state2 = 124;

    exception.expect(InvalidStateException.class);
    Precondition.state(state1).noLessThan(state2, "message");
  }

  @Test
  public void testNoLessThanCheckFailsWithNameWithMessage() {

    int state1 = 123;
    int state2 = 124;

    exception.expect(InvalidStateException.class);
    Precondition.state(state1, "name").noLessThan(state2, "message");
  }

  @Test
  public void testGreaterThanCheckSucceeds() {

    int state1 = 123;
    int state2 = 122;

    Precondition.state(state1).greaterThan(state2);
    Precondition.state(state1, "name").greaterThan(state2);
    Precondition.state(state1).greaterThan(state2, "message");
    Precondition.state(state1, "name").greaterThan(state2, "message");
  }

  @Test
  public void testGreaterThanCheckFailsNoNameNoMessage() {

    int state1 = 123;
    int state2 = 124;

    exception.expect(InvalidStateException.class);
    Precondition.state(state1).greaterThan(state2);
  }

  @Test
  public void testGreaterThanCheckFailsWithNameNoMessage() {

    int state1 = 123;
    int state2 = 124;

    exception.expect(InvalidStateException.class);
    Precondition.state(state1, "name").greaterThan(state2);
  }

  @Test
  public void testGreaterThanCheckFailsNoNameWithMessage() {

    int state1 = 123;
    int state2 = 124;

    exception.expect(InvalidStateException.class);
    Precondition.state(state1).greaterThan(state2, "message");
  }

  @Test
  public void testGreaterThanCheckFailsWithNameWithMessage() {

    int state1 = 123;
    int state2 = 124;

    exception.expect(InvalidStateException.class);
    Precondition.state(state1, "name").greaterThan(state2, "message");
  }

  @Test
  public void testGreaterThanWithEqualStatesNoNameNoMessage() {

    exception.expect(InvalidStateException.class);
    Precondition.state(123).greaterThan(123);
  }

  @Test
  public void testGreaterThanWithEqualStatesWithNameNoMessage() {

    exception.expect(InvalidStateException.class);
    Precondition.state(123, "name").greaterThan(123);
  }

  @Test
  public void testGreaterThanWithEqualStatesNoNameWithMessage() {

    exception.expect(InvalidStateException.class);
    Precondition.state(123).greaterThan(123, "message");
  }

  @Test
  public void testGreaterThanWithEqualStatesWithNameWithMessage() {

    exception.expect(InvalidStateException.class);
    Precondition.state(123, "name").greaterThan(123, "message");
  }

  @Test
  public void testNoGreaterThanCheckSucceeds() {

    int state1 = 123;
    int state2 = 124;

    Precondition.state(state1).noGreaterThan(state2);
    Precondition.state(state1, "name").noGreaterThan(state2);
    Precondition.state(state1).noGreaterThan(state2, "message");
    Precondition.state(state1, "name").noGreaterThan(state2, "message");

    state2 = 123;

    Precondition.state(state1).noGreaterThan(state2);
    Precondition.state(state1, "name").noGreaterThan(state2);
    Precondition.state(state1).noGreaterThan(state2, "message");
    Precondition.state(state1, "name").noGreaterThan(state2, "message");
  }

  @Test
  public void testNoGreaterThanCheckFailsNoNameNoMessage() {

    int state1 = 123;
    int state2 = 122;

    exception.expect(InvalidStateException.class);
    Precondition.state(state1).noGreaterThan(state2);
  }

  @Test
  public void testNoGreaterThanCheckFailsWithNameNoMessage() {

    int state1 = 123;
    int state2 = 122;

    exception.expect(InvalidStateException.class);
    Precondition.state(state1, "name").noGreaterThan(state2);
  }

  @Test
  public void testNoGreaterThanCheckFailsNoNameWithMessage() {

    int state1 = 123;
    int state2 = 122;

    exception.expect(InvalidStateException.class);
    Precondition.state(state1).noGreaterThan(state2, "message");
  }

  @Test
  public void testNoGreaterThanCheckFailsWithNameWithMessage() {

    int state1 = 123;
    int state2 = 122;

    exception.expect(InvalidStateException.class);
    Precondition.state(state1, "name").noGreaterThan(state2, "message");
  }
}
