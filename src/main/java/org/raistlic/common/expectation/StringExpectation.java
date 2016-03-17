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

package org.raistlic.common.expectation;

import org.raistlic.common.precondition.InvalidParameterException;
import org.raistlic.common.predicate.Predicates;
import org.raistlic.common.predicate.StringPredicates;

import java.util.function.Function;
import java.util.regex.Pattern;

/**
 * @author Lei Chen (2015-10-14)
 */
public class StringExpectation extends GeneralExpectation<String> {

  /**
   * Creates the {@link String} expectation instance for the {@code candidate} , which throws
   * exception exported by the {@code exceptionBuilder} in case it doesn't match the subsequent
   * checks.
   *
   * @param candidate         the candidate to be examined.
   * @param name              the name of the candidate, {@code null} if the candidate is not named.
   * @param exceptionProvider the exception builder for creating exceptions when needed, cannot be
   *                          {@code null}.
   * @throws InvalidParameterException when {@code exceptionBuilder} is {@code null}.
   */
  StringExpectation(String candidate,
                    String name,
                    Function<String, ? extends RuntimeException> exceptionProvider) {

    super(candidate, name, exceptionProvider);

    if (exceptionProvider == null) {
      throw new InvalidParameterException("'exceptionProvider' should not be null.");
    }
  }

  /**
   * The method
   */
  public void isEmpty() {

    String message = "";
    if (name() != null) {
      message = "'" + name() + "' should be empty but is: '" + getCandidate() + "'";
    }
    isEmpty(message);
  }

  public void isEmpty(String message) {

    setMessage(message);
    setPredicate(StringPredicates.isEmpty());
    evaluate();
  }

  public void isNullOrEmpty() {

    String message = "";
    if (name() != null) {
      message = "'" + name() + "' should be null or empty, but is: '" + getCandidate() + "'";
    }
    isNullOrEmpty(message);
  }

  public void isNullOrEmpty(String message) {

    setMessage(message);
    setPredicate(Predicates.or(
            Predicates.isNull(),
            StringPredicates.isEmpty()
    ));
    evaluate();
  }

  public void notEmpty() {

    String message = "";
    if (name() != null) {
      message = "'" + name() + "' should not be empty.";
    }
    notEmpty(message);
  }

  public void notEmpty(String message) {

    setMessage(message);
    setPredicate(StringPredicates.notEmpty());
    evaluate();
  }

  public void notNullOrEmpty() {

    String message = "";
    if (name() != null) {
      message = "'" + name() + "' should not be null or empty, but is : '" + getCandidate() + "'";
    }
    notNullOrEmpty(message);
  }

  public void notNullOrEmpty(String message) {

    setMessage(message);
    setPredicate(Predicates.and(
            Predicates.notNull(),
            StringPredicates.notEmpty()
    ));
    evaluate();
  }

  public void hasLength(int length) {

    String message = "";
    if (name() != null) {
      message = "'" + name() + "' should have length of " + length;
    }
    hasLength(length, message);
  }

  public void hasLength(int length, String message) {

    setMessage(message);
    setPredicate(StringPredicates.hasLength(length));
    evaluate();
  }

  public void matchesPattern(Pattern pattern) {

    if (pattern == null) {
      throw new InvalidParameterException("'pattern' should not be null.");
    }

    String message = "";
    if (name() != null) {
      message = "'" + name() + "' should match the given pattern: /" + pattern +
              "/, but does not (actual value: '" + getCandidate() + "')";
    }
    matchesPattern(pattern, message);
  }

  public void matchesPattern(Pattern pattern, String message) {

    if (pattern == null) {
      throw new InvalidParameterException("'pattern' should not be null.");
    }

    setMessage(message);
    setPredicate(StringPredicates.matchesPattern(pattern));
    evaluate();
  }

  public void matchesPattern(String pattern) {

    if (pattern == null) {
      throw new InvalidParameterException("'pattern' should not be null.");
    }
    matchesPattern(Pattern.compile(pattern));
  }

  public void matchesPattern(String pattern, String message) {

    if (pattern == null) {
      throw new InvalidParameterException("'pattern' should not be null.");
    }
    matchesPattern(Pattern.compile(pattern), message);
  }
}
