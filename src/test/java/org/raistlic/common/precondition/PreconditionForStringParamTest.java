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
 * @author Lei CHEN (2015-03-02)
 */
@RunWith(JUnit4.class)
public class PreconditionForStringParamTest {

  @Test
  public void testIsNullWithNullParam() {

    Precondition.param((String) null).isNull();
    Precondition.param((String) null, "name").isNull();
    Precondition.param((String) null).isNull("message");
    Precondition.param((String) null, "name").isNull("message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsNullWithNonNullParamNoNameNoMessage() {

    Precondition.param("abc").isNull();
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsNullWithNonNullParamWithNameNoMessage() {

    Precondition.param("abc", "name").isNull();
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsNullWithNonNullParamNoNameWithMessage() {

    Precondition.param("abc").isNull("message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsNullWithNonNullParamWithNameWithMessage() {

    Precondition.param("abc", "name").isNull("message");
  }

  @Test
  public void testNotNullWithNonNullParam() {

    Precondition.param("abc").notNull();
    Precondition.param("abc", "name").notNull();
    Precondition.param("abc").notNull("message");
    Precondition.param("abc", "name").notNull("message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testNotNullWithNullParamNoNameNoMessage() {

    Precondition.param((String)null).notNull();
  }

  @Test(expected = InvalidParameterException.class)
  public void testNotNullWithNullParamWithNameNoMessage() {

    Precondition.param((String)null, "name").notNull();
  }

  @Test(expected = InvalidParameterException.class)
  public void testNotNullWithNullParamNoNameWithMessage() {

    Precondition.param((String)null).notNull("message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testNotNullWithNullParamWithNameWithMessage() {

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

  @Test(expected = InvalidParameterException.class)
  public void testEqualToWithNotEqualParamsNoNameNoMessage() {

    String param1 = "abc";
    String param2 = "def";

    Precondition.param(param1).equalTo(param2);
  }

  @Test(expected = InvalidParameterException.class)
  public void testEqualToWithNotEqualParamsWithNameNoMessage() {

    String param1 = "abc";
    String param2 = "def";

    Precondition.param(param1, "name").equalTo(param2);
  }

  @Test(expected = InvalidParameterException.class)
  public void testEqualToWithNotEqualParamsNoNameWithMessage() {

    String param1 = "abc";
    String param2 = "def";

    Precondition.param(param1).equalTo(param2, "message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testEqualToWithNotEqualParamsWithNameWithMessage() {

    String param1 = "abc";
    String param2 = "def";

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

  @Test(expected = InvalidParameterException.class)
  public void testNotEqualToWithEqualParamsNoNameNoMessage() {

    String param1 = "abc";
    String param2 = "abc";

    Precondition.param(param1).notEqualTo(param2);
  }

  @Test(expected = InvalidParameterException.class)
  public void testNotEqualToWithEqualParamsWithNameNoMessage() {

    String param1 = "abc";
    String param2 = "abc";

    Precondition.param(param1, "name").notEqualTo(param2);
  }

  @Test(expected = InvalidParameterException.class)
  public void testNotEqualToWithEqualParamsNoNameWithMessage() {

    String param1 = "abc";
    String param2 = "abc";

    Precondition.param(param1).notEqualTo(param2, "message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testNotEqualToWithEqualParamsWithNameWithMessage() {

    String param1 = "abc";
    String param2 = "abc";

    Precondition.param(param1, "name").notEqualTo(param2, "message");
  }

  @Test
  public void testIsEmptyWithEmptyString() {

    Precondition.param("").isEmpty();
    Precondition.param("", "name").isEmpty();
    Precondition.param("").isEmpty("message");
    Precondition.param("", "name").isEmpty("message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsEmptyWithNonEmptyStringNoNameNoMessage() {

    Precondition.param("abc").isEmpty();
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsEmptyWithNonEmptyStringWithNameNoMessage() {

    Precondition.param("abc", "name").isEmpty();
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsEmptyWithNonEmptyStringNoNameWithMessage() {

    Precondition.param("abc").isEmpty("message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsEmptyWithNonEmptyStringWithNameWithMessage() {

    Precondition.param("abc", "name").isEmpty("message");
  }

  @Test
  public void testNotEmptyWithNonEmptyParam() {

    Precondition.param("abc").notEmpty();
    Precondition.param("abc", "name").notEmpty();
    Precondition.param("abc").notEmpty("message");
    Precondition.param("abc", "name").notEmpty("message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testNotEmptyWithEmptyParamNoNameNoMessage() {

    Precondition.param("").notEmpty();
  }

  @Test(expected = InvalidParameterException.class)
  public void testNotEmptyWithEmptyParamWithNameNoMessage() {

    Precondition.param("", "name").notEmpty();
  }

  @Test(expected = InvalidParameterException.class)
  public void testNotEmptyWithEmptyParamNoNameWithMessage() {

    Precondition.param("").notEmpty("message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testNotEmptyWithEmptyParamWithNameWithMessage() {

    Precondition.param("", "name").notEmpty("message");
  }
}
