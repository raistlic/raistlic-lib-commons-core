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
    Precondition.param((String) null).isNull("message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsNullWithNonNullParamNoMessage() {

    Precondition.param("abc").isNull();
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsNullWithNonNullParamWithMessage() {

    Precondition.param("abc").isNull("message");
  }

  @Test
  public void testNotNullWithNonNullParam() {

    Precondition.param("abc").isNotNull();
    Precondition.param("abc").isNotNull("message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testNotNullWithNullParamNoMessage() {

    Precondition.param((String)null).isNotNull();
  }

  @Test(expected = InvalidParameterException.class)
  public void testNotNullWithNullParamWithMessage() {

    Precondition.param((String)null).isNotNull("message");
  }

  @Test
  public void testEqualToWithEqualParams() {

    String param1 = "abc";
    String param2 = "abc";

    Precondition.param(param1).isEqualTo(param2);
    Precondition.param(param1).isEqualTo(param2, "message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testEqualToWithNotEqualParamsNoMessage() {

    String param1 = "abc";
    String param2 = "def";

    Precondition.param(param1).isEqualTo(param2);
  }

  @Test(expected = InvalidParameterException.class)
  public void testEqualToWithNotEqualParamsWithMessage() {

    String param1 = "abc";
    String param2 = "def";

    Precondition.param(param1).isEqualTo(param2, "message");
  }

  @Test
  public void testNotEqualToWithNotEqualParams() {

    String param1 = "abc";
    String param2 = "def";

    Precondition.param(param1).isNotEqualTo(param2);
    Precondition.param(param1).isNotEqualTo(param2, "message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testNotEqualToWithEqualParamsNoMessage() {

    String param1 = "abc";
    String param2 = "abc";

    Precondition.param(param1).isNotEqualTo(param2);
  }

  @Test(expected = InvalidParameterException.class)
  public void testNotEqualToWithEqualParamsWithMessage() {

    String param1 = "abc";
    String param2 = "abc";

    Precondition.param(param1).isNotEqualTo(param2, "message");
  }

  @Test
  public void testIsEmptyWithEmptyString() {

    Precondition.param("").isEmpty();
    Precondition.param("").isEmpty("message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsEmptyWithNonEmptyStringNoMessage() {

    Precondition.param("abc").isEmpty();
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsEmptyWithNonEmptyStringWithMessage() {

    Precondition.param("abc").isEmpty("message");
  }

  @Test
  public void testNotEmptyWithNonEmptyParam() {

    Precondition.param("abc").isNotEmpty();
    Precondition.param("abc").isNotEmpty("message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testNotEmptyWithEmptyParamNoMessage() {

    Precondition.param("").isNotEmpty();
  }

  @Test(expected = InvalidParameterException.class)
  public void testNotEmptyWithEmptyParamWithMessage() {

    Precondition.param("").isNotEmpty("message");
  }
}
