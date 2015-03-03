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

/**
 * @author Lei CHEN (2015-03-02)
 */
public class ParamExpectationOfIntTest {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @Test
  public void testEqualToWithEqualParams() {

    int param1 = 123;
    int param2 = 123;

    Precondition.param(param1).equalTo(param2);
    Precondition.param(param1, "name").equalTo(param2);
    Precondition.param(param1).equalTo(param2, "message");
    Precondition.param(param1, "name").equalTo(param2, "message");
  }

  @Test
  public void testEqualToWithNotEqualParamsNoNameNoMessage() {

    int param1 = 123;
    int param2 = 456;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1).equalTo(param2);
  }

  @Test
  public void testEqualToWithNotEqualParamsWithNameNoMessage() {

    int param1 = 123;
    int param2 = 456;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1, "name").equalTo(param2);
  }

  @Test
  public void testEqualToWithNotEqualParamsNoNameWithMessage() {

    int param1 = 123;
    int param2 = 456;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1).equalTo(param2, "message");
  }

  @Test
  public void testEqualToWithNotEqualParamsWithNameWithMessage() {

    int param1 = 123;
    int param2 = 456;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1, "name").equalTo(param2, "message");
  }

  @Test
  public void testNotEqualToWithNotEqualParams() {

    int param1 = 123;
    int param2 = 456;

    Precondition.param(param1).notEqualTo(param2);
    Precondition.param(param1, "name").notEqualTo(param2);
    Precondition.param(param1).notEqualTo(param2, "message");
    Precondition.param(param1, "name").notEqualTo(param2, "message");
  }

  @Test
  public void testNotEqualToWIthEqualParamsNoNameNoMessage() {

    int param1 = 123;
    int param2 = 123;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1).notEqualTo(param2);
  }

  @Test
  public void testNotEqualToWIthEqualParamsWithNameNoMessage() {

    int param1 = 123;
    int param2 = 123;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1, "name").notEqualTo(param2);
  }

  @Test
  public void testNotEqualToWIthEqualParamsNoNameWithMessage() {

    int param1 = 123;
    int param2 = 123;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1).notEqualTo(param2, "message");
  }

  @Test
  public void testNotEqualToWithEqualParamsWithNameWithMessage() {

    int param1 = 123;
    int param2 = 123;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1, "name").notEqualTo(param2, "message");
  }

  @Test
  public void testLessThanCheckSucceeds() {

    int param1 = 123;
    int param2 = 124;

    Precondition.param(param1).lessThan(param2);
    Precondition.param(param1, "name").lessThan(param2);
    Precondition.param(param1).lessThan(param2, "message");
    Precondition.param(param1, "name").lessThan(param2, "message");
  }

  @Test
  public void testLessThanCheckFailsNoNameNoMessage() {

    int param1 = 123;
    int param2 = 122;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1).lessThan(param2);
  }

  @Test
  public void testLessThanCheckFailsWithNameNoMessage() {

    int param1 = 123;
    int param2 = 122;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1, "name").lessThan(param2);
  }

  @Test
  public void testLessThanCheckFailsNoNameWithMessage() {

    int param1 = 123;
    int param2 = 122;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1).lessThan(param2, "message");
  }

  @Test
  public void testLessThanCheckFailsWithNameWithMessage() {

    int param1 = 123;
    int param2 = 122;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1, "name").lessThan(param2, "message");
  }

  @Test
  public void testLessThanWithEqualParamsNoNameNoMessage() {

    exception.expect(InvalidParameterException.class);
    Precondition.param(123).lessThan(123);
  }

  @Test
  public void testLessThanWithEqualParamsWithNameNoMessage() {

    exception.expect(InvalidParameterException.class);
    Precondition.param(123, "name").lessThan(123);
  }

  @Test
  public void testLessThanWithEqualParamsNoNameWithMessage() {

    exception.expect(InvalidParameterException.class);
    Precondition.param(123).lessThan(123, "message");
  }

  @Test
  public void testLessThanWithEqualParamsWithNameWithMessage() {

    exception.expect(InvalidParameterException.class);
    Precondition.param(123, "name").lessThan(123, "message");
  }

  @Test
  public void testNoLessThanCheckSucceeds() {

    int param1 = 123;
    int param2 = 122;

    Precondition.param(param1).noLessThan(param2);
    Precondition.param(param1, "name").noLessThan(param2);
    Precondition.param(param1).noLessThan(param2, "message");
    Precondition.param(param1, "name").noLessThan(param2, "message");

    param2 = 123;

    Precondition.param(param1).noLessThan(param2);
    Precondition.param(param1, "name").noLessThan(param2);
    Precondition.param(param1).noLessThan(param2, "message");
    Precondition.param(param1, "name").noLessThan(param2, "message");
  }

  @Test
  public void testNoLessThanCheckFailsNoNameNoMessage() {

    int param1 = 123;
    int param2 = 124;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1).noLessThan(param2);
  }

  @Test
  public void testNoLessThanCheckFailsWithNameNoMessage() {

    int param1 = 123;
    int param2 = 124;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1, "name").noLessThan(param2);
  }

  @Test
  public void testNoLessThanCheckFailsNoNameWithMessage() {

    int param1 = 123;
    int param2 = 124;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1).noLessThan(param2, "message");
  }

  @Test
  public void testNoLessThanCheckFailsWithNameWithMessage() {

    int param1 = 123;
    int param2 = 124;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1, "name").noLessThan(param2, "message");
  }

  @Test
  public void testGreaterThanCheckSucceeds() {

    int param1 = 123;
    int param2 = 122;

    Precondition.param(param1).greaterThan(param2);
    Precondition.param(param1, "name").greaterThan(param2);
    Precondition.param(param1).greaterThan(param2, "message");
    Precondition.param(param1, "name").greaterThan(param2, "message");
  }

  @Test
  public void testGreaterThanCheckFailsNoNameNoMessage() {

    int param1 = 123;
    int param2 = 124;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1).greaterThan(param2);
  }

  @Test
  public void testGreaterThanCheckFailsWithNameNoMessage() {

    int param1 = 123;
    int param2 = 124;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1, "name").greaterThan(param2);
  }

  @Test
  public void testGreaterThanCheckFailsNoNameWithMessage() {

    int param1 = 123;
    int param2 = 124;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1).greaterThan(param2, "message");
  }

  @Test
  public void testGreaterThanCheckFailsWithNameWithMessage() {

    int param1 = 123;
    int param2 = 124;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1, "name").greaterThan(param2, "message");
  }

  @Test
  public void testGreaterThanWithEqualParamsNoNameNoMessage() {

    exception.expect(InvalidParameterException.class);
    Precondition.param(123).greaterThan(123);
  }

  @Test
  public void testGreaterThanWithEqualParamsWithNameNoMessage() {

    exception.expect(InvalidParameterException.class);
    Precondition.param(123, "name").greaterThan(123);
  }

  @Test
  public void testGreaterThanWithEqualParamsNoNameWithMessage() {

    exception.expect(InvalidParameterException.class);
    Precondition.param(123).greaterThan(123, "message");
  }

  @Test
  public void testGreaterThanWithEqualParamsWithNameWithMessage() {

    exception.expect(InvalidParameterException.class);
    Precondition.param(123, "name").greaterThan(123, "message");
  }

  @Test
  public void testNoGreaterThanCheckSucceeds() {

    int param1 = 123;
    int param2 = 124;

    Precondition.param(param1).noGreaterThan(param2);
    Precondition.param(param1, "name").noGreaterThan(param2);
    Precondition.param(param1).noGreaterThan(param2, "message");
    Precondition.param(param1, "name").noGreaterThan(param2, "message");

    param2 = 123;

    Precondition.param(param1).noGreaterThan(param2);
    Precondition.param(param1, "name").noGreaterThan(param2);
    Precondition.param(param1).noGreaterThan(param2, "message");
    Precondition.param(param1, "name").noGreaterThan(param2, "message");
  }

  @Test
  public void testNoGreaterThanCheckFailsNoNameNoMessage() {

    int param1 = 123;
    int param2 = 122;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1).noGreaterThan(param2);
  }

  @Test
  public void testNoGreaterThanCheckFailsWithNameNoMessage() {

    int param1 = 123;
    int param2 = 122;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1, "name").noGreaterThan(param2);
  }

  @Test
  public void testNoGreaterThanCheckFailsNoNameWithMessage() {

    int param1 = 123;
    int param2 = 122;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1).noGreaterThan(param2, "message");
  }

  @Test
  public void testNoGreaterThanCheckFailsWithNameWithMessage() {

    int param1 = 123;
    int param2 = 122;

    exception.expect(InvalidParameterException.class);
    Precondition.param(param1, "name").noGreaterThan(param2, "message");
  }
}
