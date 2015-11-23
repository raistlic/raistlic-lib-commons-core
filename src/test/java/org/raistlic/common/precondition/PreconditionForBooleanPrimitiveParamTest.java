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
public class PreconditionForBooleanPrimitiveParamTest {

  @Test
  public void testIsTrueWithTrue() {

    Precondition.param(true).isTrue();
    Precondition.param(true, "name").isTrue();
    Precondition.param(true).isTrue("message");
    Precondition.param(true, "name").isTrue("message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsTrueWithFalseNoNameNoMessage() {

    Precondition.param(false).isTrue();
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsTrueWithFalseWithNameNoMessage() {

    Precondition.param(false, "name").isTrue();
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsTrueWithFalseNoNameWithMessage() {

    Precondition.param(false).isTrue("message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsTrueWithFalseWithNameWithMessage() {

    Precondition.param(false, "name").isTrue("message");
  }

  @Test
  public void testIsFalseWithFalse() {

    Precondition.param(false).isFalse();
    Precondition.param(false, "name").isFalse();
    Precondition.param(false).isFalse("message");
    Precondition.param(false, "name").isFalse("message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsFalseWithTrueNoNameNoMessage() {

    Precondition.param(true).isFalse();
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsFalseWithTrueWithNameNoMessage() {

    Precondition.param(true, "name").isFalse();
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsFalseWithTrueNoNameWithMessage() {

    Precondition.param(true).isFalse("message");
  }

  @Test(expected = InvalidParameterException.class)
  public void testIsFalseWithTrueWithNameWithMessage() {

    Precondition.param(true, "name").isFalse("message");
  }
}
