/*
 * Copyright 2016 Lei Chen (raistlic@gmail.com)
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

package org.raistlic.common.adt;

import org.junit.Test;
import org.raistlic.common.permutation.Factorial;
import org.raistlic.common.postcondition.Postcondition;

import java.math.BigInteger;

public class FactorialSpec {

  @Test
  public void testOf() {
    
    for (int i = 0; i < 2000; i++) {
      BigInteger expected = simpleFactorial(i);
      BigInteger actual = Factorial.of(i);

      Postcondition.assertThat(actual).isEqualTo(expected);
    }
  }
  
  private static BigInteger simpleFactorial(int number) {
    BigInteger result = BigInteger.ONE;
    for (; number > 0; number--) {
      result = result.multiply(BigInteger.valueOf(number));
    }
    return result;
  }
}
