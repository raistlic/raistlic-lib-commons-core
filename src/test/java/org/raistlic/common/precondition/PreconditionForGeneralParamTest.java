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
public class PreconditionForGeneralParamTest {

  @Test
  public void testIsNullWithNullParam() {

    Precondition.param((Object) null).isNull();
    Precondition.param((Object) null).isNull("message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsNullWithNonNullParamNoMessage() {

    Precondition.param(new Object()).isNull();
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsNullWithNonNullParamWithMessage() {

    Precondition.param(new Object()).isNull("message");
  }

  @Test
  public void testNotNullWithNonNullParameter() {

    Precondition.param(new Object()).isNotNull();
    Precondition.param(new Object()).isNotNull("message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testNotNullWithNullParamNoMessage() {

    Precondition.param((Object) null).isNotNull();
  }

  @Test(expected = InvalidParameterException.class)
  public void testNotNullWithNullParamWithMessage() {

    Precondition.param((Object) null).isNotNull("message");
  }

  @Test
  public void testEqualToWithEqualParams() {

    Object param1 = new Object();

    Precondition.param(param1).isEqualTo(param1);
    Precondition.param(param1).isEqualTo(param1, "message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testEqualToWithNotEqualParamsNoMessage() {

    Object param1 = new Object();
    Object param2 = new Object();

    Precondition.param(param1).isEqualTo(param2);
  }

  @Test(expected = InvalidParameterException.class)
  public void testEqualToWithNotEqualParamsWithMessage() {

    Object param1 = new Object();
    Object param2 = new Object();

    Precondition.param(param1).isEqualTo(param2, "message");
  }

  @Test
  public void testNotEqualToWithNotEqualParams() {

    Object param1 = new Object();
    Object param2 = new Object();

    Precondition.param(param1).isNotEqualTo(param2);
    Precondition.param(param1).isNotEqualTo(param2, "message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testNotEqualToWithEqualParamsNoMessage() {

    Object param1 = new Object();

    Precondition.param(param1).isNotEqualTo(param1);
  }

  @Test(expected = InvalidParameterException.class)
  public void testNotEqualToWithEqualParamsWithMessage() {

    Object param1 = new Object();

    Precondition.param(param1).isNotEqualTo(param1, "message");
  }
}
