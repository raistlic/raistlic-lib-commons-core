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
package org.raistlic.common.predicate;

import org.raistlic.common.precondition.InvalidParameterException;

import java.util.function.Predicate;
import java.util.regex.Pattern;

public final class StringPredicates {

  /**
   * Returns a predicate instance to test whether a {@link String} is empty. A string is considered empty if it is not
   * {@code null} and has length of {@code 0} .
   *
   * @return the predicate instance.
   */
  public static Predicate<String> isEmpty() {

    return StringIsEmptyPredicate.INSTANCE;
  }

  /**
   * Returns a predicate instance to test whether a {@link String} is not empty. A string is considered not empty if it
   * is {@code null} or its length is greater than {@code 0} .
   *
   * @return the predicate instance.
   */
  public static Predicate<String> notEmpty() {

    return Predicates.not(isEmpty());
  }

  /**
   * Returns a predicate instance to test whether a given {@link String} has the specified {@code length} .
   *
   * @param length the length to test, cannot be less than {@code 0} .
   * @return the predicate instance
   * @throws InvalidParameterException when {@code length} is invalid.
   */
  public static Predicate<String> hasLength(int length) {

    if (length < 0) {
      throw new InvalidParameterException("'length' cannot be less than 0");
    }
    return new StringLengthPredicate(length);
  }

  /**
   * Returns a predicate instance to test whether a given {@link String} candidate matches the specified regular expression
   * {@code pattern} .
   *
   * @param pattern the pattern to test candidate, cannot be {@code null}.
   * @return the predicate instance
   * @throws InvalidParameterException when {@code pattern} is {@code null}.
   */
  public static Predicate<String> matchesPattern(Pattern pattern) {

    if (pattern == null) {
      throw new InvalidParameterException("'pattern' should not be null.");
    }
    return new StringMatchesPatternPredicate(pattern);
  }

  private enum StringIsEmptyPredicate implements Predicate<String> {

    INSTANCE;

    @Override
    public boolean test(String s) {

      return "".equals(s);
    }
  }

  private static class StringLengthPredicate implements Predicate<String> {

    private final int length;

    public StringLengthPredicate(int length) {

      this.length = length;
    }

    @Override
    public boolean test(String s) {

      return s != null && s.length() == length;
    }
  }

  private static final class StringMatchesPatternPredicate implements Predicate<String> {

    private final Pattern pattern;

    private StringMatchesPatternPredicate(Pattern pattern) {

      this.pattern = pattern;
    }

    @Override
    public boolean test(String s) {

      return s != null && pattern.matcher(s).matches();
    }
  }

  private StringPredicates() {
  }
}
