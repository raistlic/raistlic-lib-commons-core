/*
 * Copyright 2020 Lei Chen (raistlic@gmail.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.raistlic.common.precondition;

import java.util.Collection;

/**
 * Shortcut for frequently used param assertions.
 */
public class Param {

  /**
   * Given {@code condition} is expected to be {@code true}, otherwise an exception with {@code message} is thrown.
   *
   * @param condition the condition that's expected to be {@code true}.
   * @param message message of the exception to be thrown when {@code condition} is not {@code true}.
   *
   * @throws InvalidParameterException when {@code condition} is not {@code true}.
   */
  public static void isTrue(boolean condition, String message) {
    Precondition.assertParam(condition, message);
  }

  /**
   * Given {@code condition} is expected to be {@code false}, otherwise an exception with {@code message} is thrown.
   *
   * @param condition the condition that's expected to be {@code false}.
   * @param message message of the exception to be thrown when {@code condition} is not {@code false}.
   *
   * @throws InvalidParameterException when {@code condition} is not {@code false}.
   */
  public static void isFalse(boolean condition, String message) {
    Precondition.assertParam(!condition, message);
  }

  /**
   * Given {@code param} is expected to be {@code null}, otherwise an exception with {@code message} is thrown.
   *
   * @param param the param that's expected to be {@code null}.
   * @param message message of the exception thrown when {@code param} is not {@code null}.
   *
   * @throws InvalidParameterException when {@code param} is not {@code null}.
   */
  public static void isNull(Object param, String message) {
    Precondition.assertParam(param == null, message);
  }

  /**
   * Given {@code param} is expected not to be {@code null}, otherwise an exception with {@code message} is thrown.
   *
   * @param param the param that's expected not to be {@code null}.
   * @param message message of the exception thrown when {@code param} is {@code null}.
   *
   * @throws InvalidParameterException when {@code param} is {@code null}.
   */
  public static void notNull(Object param, String message) {
    Precondition.assertParam(param != null, message);
  }

  /**
   * Given {@code param} is expected to be an empty {@link Collection}, otherwise an exception with {@code message} is
   * thrown. A {@code null} value is not an empty {@link Collection}.
   *
   * @param param the param that's expected to be an empty {@link Collection} .
   * @param message message of the exception thrown when {@code param} is not an empty {@link Collection} .
   *
   * @throws InvalidParameterException when {@code param} is {@code null} or not empty.d
   */
  public static void isEmpty(Collection<?> param, String message) {
    Precondition.assertParam(param != null && param.isEmpty(), message);
  }

  /**
   * Given {@code param} is expected to be an empty {@link String}, otherwise an exception with {@code message} is
   * thrown. A {@code null} value is not an empty {@link String} .
   *
   * @param param the param that's expected to be an empty {@link String}.
   * @param message message of the exception thrown when {@code param} is {@code null} or not empty.
   *
   * @throws InvalidParameterException when {@code param} is {@code null} or not empty.
   */
  public static void isEmpty(String param, String message) {
    Precondition.assertParam(param != null && param.isEmpty(), message);
  }

  /**
   * Given {@code param} is expected to be a non-empty {@link Collection}, otherwise an exception with {@code message}
   * is thrown.
   *
   * @param param the param that's expected to be a non-empty {@link Collection} .
   * @param message message of the exception thrown when {@code param} is {@code null} or empty.
   *
   * @throws InvalidParameterException when {@code param} is {@code null} or empty.
   */
  public static void notEmpty(Collection<?> param, String message) {
    Precondition.assertParam(param != null && !param.isEmpty(), message);
  }

  /**
   * Given {@code param} is expected to be a non-empty {@link String}, otherwise an exception with {@code message} is
   * thrown. A {@code null} value is not a non-empty {@link String} .
   *
   * @param param the param that's expected to be a non-empty string.
   * @param message message of the exception thrown when {@code param} is {@code null} or empty.
   *
   * @throws InvalidParameterException when {@code param} is {@code null} or empty.
   */
  public static void notEmpty(String param, String message) {
    Precondition.assertParam(param != null && param.length() > 0, message);
  }
}
