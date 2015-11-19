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

package org.raistlic.common.numbertext;

import java.util.function.Predicate;
import java.util.regex.Pattern;

/**
 * @author Lei CHEN (2015-11-19)
 */
final class LocalUtils {

  static final Pattern VALID_NUMBER_PATTERN = Pattern.compile("^\\d+(\\.\\d+)?");

  static Predicate<String> numberRangePredicate(int maxDigits) {

    assert maxDigits > 0;

    return new NumberRangePredicate(maxDigits);
  }

  private static class NumberRangePredicate implements Predicate<String> {

    private final int maxDigits;

    private NumberRangePredicate(int maxDigits) {

      assert maxDigits > 0;

      this.maxDigits = maxDigits;
    }

    @Override
    public boolean test(String s) {

      int index = s.indexOf('.');
      if (index < 0) {
        return s.length() <= maxDigits;
      }
      else {
        return index <= maxDigits;
      }
    }

    @Override
    public String toString() {

      return "Number maximum digits before floating point is: " + maxDigits;
    }
  }

  private LocalUtils() { }
}
