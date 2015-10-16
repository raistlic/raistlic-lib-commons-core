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
    Precondition.param((Object) null, "name").isNull();
    Precondition.param((Object) null).isNull("message");
    Precondition.param((Object) null, "name").isNull("message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsNullWithNonNullParamNoNameNoMessage() {

    Precondition.param(new Object()).isNull();
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsNullWithNonNullParamWithNameNoMessage() {

    Precondition.param(new Object(), "name").isNull();
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsNullWithNonNullParamNoNameWithMessage() {

    Precondition.param(new Object()).isNull("message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsNullWithNonNullParamWithNameWithMessage() {

    Precondition.param(new Object(), "name").isNull("message");
  }

  @Test
  public void testNotNullWithNonNullParameter() {

    Precondition.param(new Object()).notNull();
    Precondition.param(new Object()).notNull("message");
    Precondition.param(new Object(), "name").notNull();
    Precondition.param(new Object(), "name").notNull("message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testNotNullWithNullParamNoNameNoMessage() {

    Precondition.param((Object) null).notNull();
  }

  @Test(expected = InvalidParameterException.class)
  public void testNotNullWithNullParamWithNameNoMessage() {

    Precondition.param((Object) null, "name").notNull();
  }

  @Test(expected = InvalidParameterException.class)
  public void testNotNullWithNullParamNoNameWithMessage() {

    Precondition.param((Object) null).notNull("message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testNotNullWithNullParamWithNameWithMessage() {

    Precondition.param((Object) null, "name").notNull("message");
  }

  @Test
  public void testEqualToWithEqualParams() {

    Object param1 = new Object();

    Precondition.param(param1).equalTo(param1);
    Precondition.param(param1, "name").equalTo(param1);
    Precondition.param(param1).equalTo(param1, "message");
    Precondition.param(param1, "name").equalTo(param1, "message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testEqualToWithNotEqualParamsNoNameNoMessage() {

    Object param1 = new Object();
    Object param2 = new Object();

    Precondition.param(param1).equalTo(param2);
  }

  @Test(expected = InvalidParameterException.class)
  public void testEqualToWithNotEqualParamsWithNameNoMessage() {

    Object param1 = new Object();
    Object param2 = new Object();

    Precondition.param(param1, "name").equalTo(param2);
  }

  @Test(expected = InvalidParameterException.class)
  public void testEqualToWithNotEqualParamsNoNameWithMessage() {

    Object param1 = new Object();
    Object param2 = new Object();

    Precondition.param(param1).equalTo(param2, "message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testEqualToWithNotEqualParamsWithNameWithMessage() {

    Object param1 = new Object();
    Object param2 = new Object();

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

  @Test(expected = InvalidParameterException.class)
  public void testNotEqualToWithEqualParamsNoNameNoMessage() {

    Object param1 = new Object();

    Precondition.param(param1).notEqualTo(param1);
  }

  @Test(expected = InvalidParameterException.class)
  public void testNotEqualToWithEqualParamsWithNameNoMessage() {

    Object param1 = new Object();

    Precondition.param(param1, "name").notEqualTo(param1);
  }

  @Test(expected = InvalidParameterException.class)
  public void testNotEqualToWithEqualParamsNoNameWithMessage() {

    Object param1 = new Object();

    Precondition.param(param1).notEqualTo(param1, "message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testNotEqualToWithEqualParamsWithNameWithMessage() {

    Object param1 = new Object();

    Precondition.param(param1, "name").notEqualTo(param1, "message");
  }
}
