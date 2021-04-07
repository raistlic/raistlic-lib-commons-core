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

package org.raistlic.common.util;

import org.raistlic.common.precondition.Param;

/**
 * Holds frequently used utility methods for numbers.
 */
public final class Numbers {

  /**
   * Confine the specified {@code value} into the range with {@code min} and {@code max} value, inclusively.
   *
   * @param value the value to be confined into specified range, cannot be {@code null}.
   * @param min   min value of the range to confine the value, cannot be {@code null}.
   * @param max   max value of the range to confine the value, cannot be {@code null} or less than {@code min}.
   * @param <N>   the number type.
   * @return the confined value.
   * @throws org.raistlic.common.precondition.InvalidParameterException when any of the parameters are invalid.
   */
  public static <N extends Number & Comparable<N>> N confine(N value, N min, N max) {

    Param.notNull(value, "value cannot be null");
    Param.notNull(min, "min cannot be null");
    Param.notNull(max, "max cannot be null");
    Param.isTrue(max.compareTo(min) >= 0, "max cannot be less than min");

    if (value.compareTo(min) < 0) {
      value = min;
    } else if (value.compareTo(max) > 0) {
      value = max;
    }
    return value;
  }

  /**
   * Confine the specified {@code int} {@code value} into the range with {@code min} and {@code max} value, inclusively.
   *
   * @param value the value to be confined into specified range.
   * @param min   min value of the range to confine the value.
   * @param max   max value of the range to confine the value, cannot be less than {@code min}.
   * @return the confined value.
   * @throws org.raistlic.common.precondition.InvalidParameterException when any of the parameters are invalid.
   */
  public static int confine(int value, int min, int max) {

    Param.isTrue(max >= min, "max cannot be less than min");

    value = Math.max(min, value);
    return Math.min(value, max);
  }

  /**
   * Confine the specified {@code long} {@code value} into the range with {@code min} and {@code max} value, inclusively.
   *
   * @param value the value to be confined into specified range.
   * @param min   min value of the range to confine the value.
   * @param max   max value of the range to confine the value, cannot be less than {@code min}.
   * @return the confined value.
   * @throws org.raistlic.common.precondition.InvalidParameterException when any of the parameters are invalid.
   */
  public static long confine(long value, long min, long max) {

    Param.isTrue(max >= min, "max cannot be less than min");

    value = Math.max(min, value);
    return Math.min(value, max);
  }

  /**
   * Confine the specified {@code float} {@code value} into the range with {@code min} and {@code max} value, inclusively.
   *
   * @param value the value to be confined into specified range.
   * @param min   min value of the range to confine the value.
   * @param max   max value of the range to confine the value, cannot be less than {@code min}.
   * @return the confined value.
   * @throws org.raistlic.common.precondition.InvalidParameterException when any of the parameters are invalid.
   */
  public static float confine(float value, float min, float max) {

    Param.isTrue(max >= min, "max cannot be less than min");

    value = Math.max(min, value);
    return Math.min(value, max);
  }

  /**
   * Confine the specified {@code double} {@code value} into the range with {@code min} and {@code max} value, inclusively.
   *
   * @param value the value to be confined into specified range.
   * @param min   min value of the range to confine the value.
   * @param max   max value of the range to confine the value, cannot be less than {@code min}.
   * @return the confined value.
   * @throws org.raistlic.common.precondition.InvalidParameterException when any of the parameters are invalid.
   */
  public static double confine(double value, double min, double max) {

    Param.isTrue(max >= min, "max cannot be less than min");

    value = Math.max(min, value);
    return Math.min(value, max);
  }

  private Numbers() {
  }
}
