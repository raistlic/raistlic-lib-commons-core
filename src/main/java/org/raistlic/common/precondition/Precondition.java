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
 * @author Lei.C (2015-02-13)
 */
public final class Precondition {

  public static Expectation.OfObject param(Object parameter) {

    return new ParamExpectation.OfObject(parameter, null);
  }

  public static Expectation.OfObject param(Object parameter, String name) {

    return new ParamExpectation.OfObject(parameter, name);
  }

//  public static BooleanExpectation param(boolean evaluative) {
//
//    return new ParamBooleanExpectation(evaluative, null);
//  }
//
//  public static BooleanExpectation param(boolean evaluative, String message) {
//
//    return new ParamBooleanExpectation(evaluative, message);
//  }

  public static void assertTrue(boolean assertion) {

  }

  public static void assertTrue(boolean assertion, String message) {

  }

  public static void assertFalse(boolean assertion) {

  }

  public static void assertFalse(boolean assertion, String message) {

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
