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
 * @author Lei CHEN (2015-03-02)
 */
@RunWith(JUnit4.class)
public class PreconditionForNumberParamTest {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @Test
  public void testEqualToWithEqualParams() {

    int param1 = 123;
    int param2 = 123;

    Precondition.param(param1).isEqualTo(param2);
    Precondition.param(param1).isEqualTo(param2, "message");
  }

  @Test
  public void testEqualToWithNotEqualParamsNoMessage() {

    int param1 = 123;
    int param2 = 456;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1).isEqualTo(param2);
  }

  @Test
  public void testEqualToWithNotEqualParamsWithMessage() {

    int param1 = 123;
    int param2 = 456;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1).isEqualTo(param2, "message");
  }

  @Test
  public void testNotEqualToWithNotEqualParams() {

    int param1 = 123;
    int param2 = 456;

    Precondition.param(param1).isNotEqualTo(param2);
    Precondition.param(param1).isNotEqualTo(param2, "message");
  }

  @Test
  public void testNotEqualToWIthEqualParamsNoMessage() {

    int param1 = 123;
    int param2 = 123;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1).isNotEqualTo(param2);
  }

  @Test
  public void testNotEqualToWIthEqualParamsWithMessage() {

    int param1 = 123;
    int param2 = 123;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1).isNotEqualTo(param2, "message");
  }

  @Test
  public void testLessThanCheckSucceeds() {

    int param1 = 123;
    int param2 = 124;

    Precondition.param(param1).lessThan(param2);
    Precondition.param(param1).lessThan(param2, "message");
  }

  @Test
  public void testLessThanCheckFailsNoMessage() {

    int param1 = 123;
    int param2 = 122;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1).lessThan(param2);
  }

  @Test
  public void testLessThanCheckFailsWithMessage() {

    int param1 = 123;
    int param2 = 122;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1).lessThan(param2, "message");
  }

  @Test
  public void testLessThanWithEqualParamsNoMessage() {

    exception.expect(InvalidParameterException.class);
    Precondition.param(123).lessThan(123);
  }

  @Test
  public void testLessThanWithEqualParamsWithMessage() {

    exception.expect(InvalidParameterException.class);
    Precondition.param(123).lessThan(123, "message");
  }

  @Test
  public void testGreaterThanOrEqualToCheckSucceeds() {

    int param1 = 123;
    int param2 = 122;

    Precondition.param(param1).greaterThanOrEqualTo(param2);
    Precondition.param(param1).greaterThanOrEqualTo(param2, "message");

    param2 = 123;

    Precondition.param(param1).greaterThanOrEqualTo(param2);
    Precondition.param(param1).greaterThanOrEqualTo(param2, "message");
  }

  @Test
  public void testGreaterThanOrEqualToCheckFailsNoMessage() {

    int param1 = 123;
    int param2 = 124;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1).greaterThanOrEqualTo(param2);
  }

  @Test
  public void testGreaterThanOrEqualToCheckFailsWithMessage() {

    int param1 = 123;
    int param2 = 124;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1).greaterThanOrEqualTo(param2, "message");
  }

  @Test
  public void testGreaterThanCheckSucceeds() {

    int param1 = 123;
    int param2 = 122;

    Precondition.param(param1).greaterThan(param2);
    Precondition.param(param1).greaterThan(param2, "message");
  }

  @Test
  public void testGreaterThanCheckFailsNoMessage() {

    int param1 = 123;
    int param2 = 124;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1).greaterThan(param2);
  }

  @Test
  public void testGreaterThanCheckFailsWithMessage() {

    int param1 = 123;
    int param2 = 124;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1).greaterThan(param2, "message");
  }

  @Test
  public void testGreaterThanWithEqualParamsNoMessage() {

    exception.expect(InvalidParameterException.class);
    Precondition.param(123).greaterThan(123);
  }

  @Test
  public void testGreaterThanWithEqualParamsWithMessage() {

    exception.expect(InvalidParameterException.class);
    Precondition.param(123).greaterThan(123, "message");
  }

  @Test
  public void testLessThanOrEqualToCheckSucceeds() {

    int param1 = 123;
    int param2 = 124;

    Precondition.param(param1).lessThanOrEqualTo(param2);
    Precondition.param(param1).lessThanOrEqualTo(param2, "message");

    param2 = 123;

    Precondition.param(param1).lessThanOrEqualTo(param2);
    Precondition.param(param1).lessThanOrEqualTo(param2, "message");
  }

  @Test
  public void testLessThanOrEqualToCheckFailsNoMessage() {

    int param1 = 123;
    int param2 = 122;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1).lessThanOrEqualTo(param2);
  }

  @Test
  public void testLessThanOrEqualToCheckFailsWithMessage() {

    int param1 = 123;
    int param2 = 122;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1).lessThanOrEqualTo(param2, "message");
  }
}
