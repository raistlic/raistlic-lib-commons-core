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

package org.raistlic.common.permutation;

import org.raistlic.common.precondition.Param;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

/**
 * A factorial calculator.
 */
public class Factorial {

  /**
   * Returns the factorial of {@code number} .
   * 
   * @param number the number of which to calculate the factorial, cannot be negative.
   * @return the factorial result of the specified {@code number}, as a {@code BigInteger}.
   * 
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code number < 0}.
   */
  public static BigInteger of(int number) {

    Param.isTrue(number >= 0, "number cannot be less than 0");

    return Optional.ofNullable(cache.get(number))
      .orElse(calculateAndCache(number));
  }

  private static BigInteger calculateAndCache(int number) {

    BigInteger result = BigInteger.ONE;
    for (int i = 2; i <= number; i++) {
      result = result.multiply(BigInteger.valueOf(i));
      cache.put(i, result);
    }
    return result;
  }

  private static final Map<Integer, BigInteger> cache = new HashMap<>();

  /*
   * Functionality served via static method, this class is designed not to be 
   * inherited or instantiated.
   */
  private Factorial() {}
}
