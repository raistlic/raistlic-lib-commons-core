/*
 * Copyright 2013 Lei CHEN (raistlic@gmail.com)
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

import java.math.BigInteger;
import org.raistlic.common.adt.WeakArray;

/**
 * This class defines a tool to calculate mathematical factorial results of 
 * positive integers.
 * 
 * @author Lei.C
 */
public class Factorial {
  
  /**
   * This method returns the mathematical result of {@code number!}.
   * 
   * @param number the number of which to calculate the factorial, cannot be 
   *        negative.
   * 
   * @return the factorial result of the specified {@code number}, as a 
   *         {@code BigInteger}.
   * 
   * @throws IllegalArgumentException if {@code number < 0}.
   */
  public static BigInteger of(int number) {

    if( number < 0 )
      throw new IllegalArgumentException();
    
    int index = Math.min(RESULTS.length()-1, number);
    BigInteger result = BigInteger.ONE;
    
    while (index > 1) {
      
      result = RESULTS.get(index);
      if( result != null )
        break;
      index--;
    }
    
    for (int i = index + 1; i <= number; i++) {
      
      result = result.multiply(BigInteger.valueOf(i));
      if( i < RESULTS.length() )
        RESULTS.set(i, result);
    }
    return result;
  }
  
  /*
   * An array to cache some results.
   */
  private static final WeakArray<BigInteger> RESULTS = 
          new WeakArray<BigInteger>(1024);
  
  /*
   * Functionality served via static method, this class is designed not to be 
   * inherited or instantiated.
   */
  private Factorial() {}
}
