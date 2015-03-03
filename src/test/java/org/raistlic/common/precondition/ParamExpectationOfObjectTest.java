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
public class ParamExpectationOfObjectTest {

  @Rule
  public ExpectedException thrown = ExpectedException.none();

  @Test
  public void testIsNullWithNullParam() {

    Precondition.param(null).isNull();
    Precondition.param(null, "name").isNull();
    Precondition.param(null).isNull("message");
    Precondition.param(null, "name").isNull("message");
  }

  @Test
  public void testIsNullWithNonNullParamNoNameNoMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param(new Object()).isNull();
  }

  @Test
  public void testIsNullWithNonNullParamWithNameNoMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param(new Object(), "name").isNull();
  }

  @Test
  public void testIsNullWithNonNullParamNoNameWithMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param(new Object()).isNull("message");
  }

  @Test
  public void testIsNullWithNonNullParamWithNameWithMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param(new Object(), "name").isNull("message");
  }

  @Test
  public void testNotNullWithNonNullParameter() {

    Precondition.param(new Object()).notNull();
    Precondition.param(new Object()).notNull("message");
    Precondition.param(new Object(), "name").notNull();
    Precondition.param(new Object(), "name").notNull("message");
  }

  @Test
  public void testNotNullWithNullParamNoNameNoMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param(null).notNull();
  }

  @Test
  public void testNotNullWithNullParamWithNameNoMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param(null, "name").notNull();
  }

  @Test
  public void testNotNullWithNullParamNoNameWithMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param(null).notNull("message");
  }

  @Test
  public void testNotNullWithNullParamWithNameWithMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param(null, "name").notNull("message");
  }

  @Test
  public void testEqualToWithEqualParams() {

    Object param1 = new Object();

    Precondition.param(param1).equalTo(param1);
    Precondition.param(param1, "name").equalTo(param1);
    Precondition.param(param1).equalTo(param1, "message");
    Precondition.param(param1, "name").equalTo(param1, "message");
  }

  @Test
  public void testEqualToWithNotEqualParamsNoNameNoMessage() {

    Object param1 = new Object();
    Object param2 = new Object();

    thrown.expect(InvalidParameterException.class);
    Precondition.param(param1).equalTo(param2);
  }

  @Test
  public void testEqualToWithNotEqualParamsWithNameNoMessage() {

    Object param1 = new Object();
    Object param2 = new Object();

    thrown.expect(InvalidParameterException.class);
    Precondition.param(param1, "name").equalTo(param2);
  }

  @Test
  public void testEqualToWithNotEqualParamsNoNameWithMessage() {

    Object param1 = new Object();
    Object param2 = new Object();

    thrown.expect(InvalidParameterException.class);
    Precondition.param(param1).equalTo(param2, "message");
  }

  @Test
  public void testEqualToWithNotEqualParamsWithNameWithMessage() {

    Object param1 = new Object();
    Object param2 = new Object();

    thrown.expect(InvalidParameterException.class);
    Precondition.param(param1, "name").equalTo(param2, "message");
  }

  @Test
  public void testNotEqualToWithNotEqualParams() {

    Object param1 = new Object();
    Object param2 = new Object();

    Precondition.param(param1).notEqualTo(param2);
    Precondition.param(param1, "name").notEqualTo(param2);
    Precondition.param(param1).notEqualTo(param2, "message");
    Precondition.param(param1, "name").notEqualTo(param2, "message");
  }

  @Test
  public void testNotEqualToWithEqualParamsNoNameNoMessage() {

    Object param1 = new Object();

    thrown.expect(InvalidParameterException.class);
    Precondition.param(param1).notEqualTo(param1);
  }

  @Test
  public void testNotEqualToWithEqualParamsWithNameNoMessage() {

    Object param1 = new Object();

    thrown.expect(InvalidParameterException.class);
    Precondition.param(param1, "name").notEqualTo(param1);
  }

  @Test
  public void testNotEqualToWithEqualParamsNoNameWithMessage() {

    Object param1 = new Object();

    thrown.expect(InvalidParameterException.class);
    Precondition.param(param1).notEqualTo(param1, "message");
  }

  @Test
  public void testNotEqualToWithEqualParamsWithNameWithMessage() {

    Object param1 = new Object();

    thrown.expect(InvalidParameterException.class);
    Precondition.param(param1, "name").notEqualTo(param1, "message");
  }
}
