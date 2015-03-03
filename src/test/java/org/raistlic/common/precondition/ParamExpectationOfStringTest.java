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
public class ParamExpectationOfStringTest {

  @Rule
  public ExpectedException thrown = ExpectedException.none();

  @Test
  public void testIsNullWithNullParam() {

    String param = null;

    Precondition.param(param).isNull();
    Precondition.param(param, "name").isNull();
    Precondition.param(param).isNull("message");
    Precondition.param(param, "name").isNull("message");
  }

  @Test
  public void testIsNullWithNonNullParamNoNameNoMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param("abc").isNull();
  }

  @Test
  public void testIsNullWithNonNullParamWithNameNoMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param("abc", "name").isNull();
  }

  @Test
  public void testIsNullWithNonNullParamNoNameWithMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param("abc").isNull("message");
  }

  @Test
  public void testIsNullWithNonNullParamWithNameWithMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param("abc", "name").isNull("message");
  }

  @Test
  public void testNotNullWithNonNullParam() {

    Precondition.param("abc").notNull();
    Precondition.param("abc", "name").notNull();
    Precondition.param("abc").notNull("message");
    Precondition.param("abc", "name").notNull("message");
  }

  @Test
  public void testNotNullWithNullParamNoNameNoMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param((String)null).notNull();
  }

  @Test
  public void testNotNullWithNullParamWithNameNoMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param((String)null, "name").notNull();
  }

  @Test
  public void testNotNullWithNullParamNoNameWithMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param((String)null).notNull("message");
  }

  @Test
  public void testNotNullWithNullParamWithNameWithMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param((String)null, "name").notNull("message");
  }

  @Test
  public void testEqualToWithEqualParams() {

    String param1 = "abc";
    String param2 = "abc";

    Precondition.param(param1).equalTo(param2);
    Precondition.param(param1, "name").equalTo(param2);
    Precondition.param(param1).equalTo(param2, "message");
    Precondition.param(param1, "name").equalTo(param2, "message");
  }

  @Test
  public void testEqualToWithNotEqualParamsNoNameNoMessage() {

    String param1 = "abc";
    String param2 = "def";

    thrown.expect(InvalidParameterException.class);
    Precondition.param(param1).equalTo(param2);
  }

  @Test
  public void testEqualToWithNotEqualParamsWithNameNoMessage() {

    String param1 = "abc";
    String param2 = "def";

    thrown.expect(InvalidParameterException.class);
    Precondition.param(param1, "name").equalTo(param2);
  }

  @Test
  public void testEqualToWithNotEqualParamsNoNameWithMessage() {

    String param1 = "abc";
    String param2 = "def";

    thrown.expect(InvalidParameterException.class);
    Precondition.param(param1).equalTo(param2, "message");
  }

  @Test
  public void testEqualToWithNotEqualParamsWithNameWithMessage() {

    String param1 = "abc";
    String param2 = "def";

    thrown.expect(InvalidParameterException.class);
    Precondition.param(param1, "name").equalTo(param2, "message");
  }

  @Test
  public void testNotEqualToWithNotEqualParams() {

    String param1 = "abc";
    String param2 = "def";

    Precondition.param(param1).notEqualTo(param2);
    Precondition.param(param1, "name").notEqualTo(param2);
    Precondition.param(param1).notEqualTo(param2, "message");
    Precondition.param(param1, "name").notEqualTo(param2, "message");
  }

  @Test
  public void testNotEqualToWithEqualParamsNoNameNoMessage() {

    String param1 = "abc";
    String param2 = "abc";

    thrown.expect(InvalidParameterException.class);
    Precondition.param(param1).notEqualTo(param2);
  }

  @Test
  public void testNotEqualToWithEqualParamsWithNameNoMessage() {

    String param1 = "abc";
    String param2 = "abc";

    thrown.expect(InvalidParameterException.class);
    Precondition.param(param1, "name").notEqualTo(param2);
  }

  @Test
  public void testNotEqualToWithEqualParamsNoNameWithMessage() {

    String param1 = "abc";
    String param2 = "abc";

    thrown.expect(InvalidParameterException.class);
    Precondition.param(param1).notEqualTo(param2, "message");
  }

  @Test
  public void testNotEqualToWithEqualParamsWithNameWithMessage() {

    String param1 = "abc";
    String param2 = "abc";

    thrown.expect(InvalidParameterException.class);
    Precondition.param(param1, "name").notEqualTo(param2, "message");
  }

  @Test
  public void testIsEmptyWithEmptyString() {

    Precondition.param("").isEmpty();
    Precondition.param("", "name").isEmpty();
    Precondition.param("").isEmpty("message");
    Precondition.param("", "name").isEmpty("message");
  }

  @Test
  public void testIsEmptyWithNonEmptyStringNoNameNoMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param("abc").isEmpty();
  }

  @Test
  public void testIsEmptyWithNonEmptyStringWithNameNoMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param("abc", "name").isEmpty();
  }

  @Test
  public void testIsEmptyWithNonEmptyStringNoNameWithMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param("abc").isEmpty("message");
  }

  @Test
  public void testIsEmptyWithNonEmptyStringWithNameWithMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param("abc", "name").isEmpty("message");
  }

  @Test
  public void testNotEmptyWithNonEmptyParam() {

    Precondition.param("abc").notEmpty();
    Precondition.param("abc", "name").notEmpty();
    Precondition.param("abc").notEmpty("message");
    Precondition.param("abc", "name").notEmpty("message");
  }

  @Test
  public void testNotEmptyWithEmptyParamNoNameNoMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param("").notEmpty();
  }

  @Test
  public void testNotEmptyWithEmptyParamWithNameNoMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param("", "name").notEmpty();
  }

  @Test
  public void testNotEmptyWithEmptyParamNoNameWithMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param("").notEmpty("message");
  }

  @Test
  public void testNotEmptyWithEmptyParamWithNameWithMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param("", "name").notEmpty("message");
  }
}
