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

import org.raistlic.common.adt.WeakArray;
import org.raistlic.common.precondition.Precondition;

import java.math.BigInteger;

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

    Precondition.param(number).greaterThanOrEqualTo(0);
    
    int index = Math.min(RESULTS.length()-1, number), limit = index;
    BigInteger result = null;
    while (index > 1) {
      result = RESULTS.get(index);
      if( result != null )
        break;
      index--;
    }
    if (result == null) {
      result = BigInteger.ONE;
    }
    
    for (++index; index <= limit; index++) {
      result = result.multiply(BigInteger.valueOf(index));
      RESULTS.set(index, result);
    }
    for (; index <= number; index++) {
      result = result.multiply(BigInteger.valueOf(index));
    }
    return result;
  }

  /*
   * An array to cache some results.
   */
  private static final WeakArray<BigInteger> RESULTS = new WeakArray<>(1024);

  /*
   * Functionality served via static method, this class is designed not to be 
   * inherited or instantiated.
   */
  private Factorial() {}
}
