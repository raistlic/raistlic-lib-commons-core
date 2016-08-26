/*
 * Copyright 2016 Lei CHEN (raistlic@gmail.com)
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
import org.raistlic.common.precondition.Precondition;
import org.raistlic.common.predicate.Predicates;
import org.raistlic.common.predicate.StringPredicates;

import java.util.function.Function;
import java.util.function.Predicate;
import java.util.regex.Pattern;

/**
 * @author Lei Chen (2015-10-14)
 */
final class StringExpectationDefault extends GenericExpectationAbstract<String, StringExpectation>
    implements StringExpectation {

  private final String candidate;

  private final Function<String, ? extends RuntimeException> exceptionMapper;

  /**
   * Creates the {@link String} expectation instance for the {@code candidate} , which throws
   * exception exported by the {@code exceptionBuilder} in case it doesn't match the subsequent
   * checks.
   *
   * @param candidate         the candidate to be examined.
   * @param exceptionMapper the exception builder for creating exceptions when needed, cannot be
   *                          {@code null}.
   * @throws InvalidParameterException when {@code exceptionMapper} is {@code null}.
   */
  StringExpectationDefault(String candidate,
                           Function<String, ? extends RuntimeException> exceptionMapper) {

    Precondition.assertParam(exceptionMapper != null, "'exceptionMapper' cannot be null.");

    this.candidate = candidate;
    this.exceptionMapper = exceptionMapper;
  }

  @Override
  StringExpectation getThis() {

    return this;
  }

  @Override
  String getCandidate() {

    return candidate;
  }

  @Override
  Function<String, ? extends RuntimeException> getExceptionMapper() {

    return exceptionMapper;
  }

  @Override
  public StringExpectation isEmpty() {

    if (IS_NOT_EMPTY.test(getCandidate())) {
      String message = "Candidate should be empty, but is not, instead it is '" + getCandidate() + "'";
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }

  @Override
  public StringExpectation isEmpty(String message) {

    if (IS_NOT_EMPTY.test(getCandidate())) {
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }

  @Override
  public StringExpectation isNullOrEmpty() {

    if (IS_NOT_NULL_OR_EMPTY.test(getCandidate())) {
      String message = "Candidate should be null or empty, but it is not, instead it is '" + getCandidate() + "'";
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }

  @Override
  public StringExpectation isNullOrEmpty(String message) {

    if (IS_NOT_NULL_OR_EMPTY.test(message)) {
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }

  @Override
  public StringExpectation isNotEmpty() {

    if (IS_EMPTY.test(getCandidate())) {
      String message = "Candidate is (unexpectedly) empty.";
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }

  @Override
  public StringExpectation isNotEmpty(String message) {

    if (IS_EMPTY.test(getCandidate())) {
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }

  @Override
  public StringExpectation isNotNullOrEmpty() {

    if (IS_NULL_OR_EMPTY.test(getCandidate())) {
      String message = "Candidate should not be null or empty, but is '" + getCandidate() + "'";
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }

  @Override
  public StringExpectation isNotNullOrEmpty(String message) {

    if (IS_NULL_OR_EMPTY.test(getCandidate())) {
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }

  @Override
  public StringExpectation hasLength(int length) {

    if (!StringPredicates.hasLength(length).test(getCandidate())) {
      String message = "Candidate '" + getCandidate() + "' does not have a length of " + length;
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }

  @Override
  public StringExpectation hasLength(int length, String message) {

    if (!StringPredicates.hasLength(length).test(getCandidate())) {
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }

  @Override
  public StringExpectation matchesPattern(Pattern pattern) {

    if (!StringPredicates.matchesPattern(pattern).test(getCandidate())) {
      String message = "Candidate '" + getCandidate() + "' does not match the pattern " + pattern;
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }

  @Override
  public StringExpectation matchesPattern(Pattern pattern, String message) {

    if (!StringPredicates.matchesPattern(pattern).test(getCandidate())) {
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }

  @Override
  public StringExpectation matchesPattern(String pattern) {

    Precondition.assertParam(pattern != null, "'pattern' should not be null, but it is.");

    return matchesPattern(Pattern.compile(pattern));
  }

  @Override
  public StringExpectation matchesPattern(String pattern, String message) {

    Precondition.assertParam(pattern != null, "'pattern' should not be null, but it is.");

    return matchesPattern(Pattern.compile(pattern), message);
  }

  private static final Predicate<? super String> IS_EMPTY = StringPredicates.isEmpty();

  private static final Predicate<? super String> IS_NOT_EMPTY = Predicates.not(StringPredicates.isEmpty());

  private static final Predicate<? super String> IS_NULL_OR_EMPTY = Predicates.or(
      Predicates.isNull(), StringPredicates.isEmpty()
  );

  private static final Predicate<? super String> IS_NOT_NULL_OR_EMPTY = Predicates.not(Predicates.or(
      Predicates.isNull(), StringPredicates.isEmpty()
  ));
}
