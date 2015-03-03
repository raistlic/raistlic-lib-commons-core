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

/**
 * @author Lei CHEN (2015-02-13)
 * @since 1.2
 */
public final class Precondition {

  public static ParamExpectation.OfObject param(Object evaluative) {

    return new ParamExpectation.OfObject(evaluative, null);
  }

  public static ParamExpectation.OfObject param(Object evaluative, String name) {

    return new ParamExpectation.OfObject(evaluative, name);
  }

  public static ParamExpectation.OfString param(String evaluative) {

    return new ParamExpectation.OfString(evaluative, null);
  }

  public static ParamExpectation.OfString param(String evaluative, String name) {

    return new ParamExpectation.OfString(evaluative, name);
  }

  public static ParamExpectation.OfBoolean param(boolean evaluative) {

    return new ParamExpectation.OfBoolean(evaluative, null);
  }

  public static ParamExpectation.OfBoolean param(boolean evaluative, String name) {

    return new ParamExpectation.OfBoolean(evaluative, name);
  }

  public static ParamExpectation.OfInt param(int evaluative) {

    return new ParamExpectation.OfInt(evaluative, null);
  }

  public static ParamExpectation.OfInt param(int evaluative, String name) {

    return new ParamExpectation.OfInt(evaluative, name);
  }

  public static void assertTrue(boolean evaluative) {

    param(evaluative).isTrue();
  }

  public static void assertTrue(boolean evaluative, String message) {

    param(evaluative).isTrue(message);
  }

  public static void assertFalse(boolean evaluative) {

    param(evaluative).isFalse();
  }

  public static void assertFalse(boolean evaluative, String message) {

    param(evaluative).isFalse(message);
  }

  public static void notNull(Object parameter) {

    param(parameter).notNull();
  }

  public static void notNull(Object parameter, String message) {

    param(parameter).notNull(message);
  }

  public static void isNull(Object parameter) {

    param(parameter).isNull();
  }

  public static void isNull(Object parameter, String message) {

    param(parameter).isNull(message);
  }

  private Precondition() { }
}
