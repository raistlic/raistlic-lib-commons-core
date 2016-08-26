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

import org.raistlic.common.precondition.Precondition;
import org.raistlic.common.predicate.NumberPredicates;

import java.util.function.Function;

/**
 * @author Lei Chen (2015-10-16)
 */
final class NumberExpectationDefault<N extends Number & Comparable<N>>
    extends GenericExpectationAbstract<N, NumberExpectation<N>> implements NumberExpectation<N> {

  private final N candidate;

  private final Function<String, ? extends RuntimeException> exceptionMapper;

  NumberExpectationDefault(N candidate, Function<String, ? extends RuntimeException> exceptionMapper) {

    Precondition.assertParam(exceptionMapper != null, "exceptionMapper cannot be null.");

    this.candidate = candidate;
    this.exceptionMapper = exceptionMapper;
  }

  @Override
  NumberExpectation<N> getThis() {

    return this;
  }

  @Override
  N getCandidate() {

    return candidate;
  }

  @Override
  Function<String, ? extends RuntimeException> getExceptionMapper() {

    return exceptionMapper;
  }

  @Override
  public NumberExpectation<N> greaterThan(N target) {

    if (!NumberPredicates.greaterThan(target).test(getCandidate())) {
      String message = "'" + getCandidate() + "' should be greater than '" + target + "', but is not.";
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }

  @Override
  public NumberExpectation<N> greaterThan(N target, String message) {

    if (!NumberPredicates.greaterThan(target).test(getCandidate())) {
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }

  @Override
  public NumberExpectation<N> greaterThanOrEqualTo(N target) {

    if (!NumberPredicates.greaterThanOrEqualTo(target).test(getCandidate())) {
      String message = "'" + getCandidate() + "' should be greater than or equal to '" + target + "', but is not.";
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }

  @Override
  public NumberExpectation<N> greaterThanOrEqualTo(N target, String message) {

    if (!NumberPredicates.greaterThanOrEqualTo(target).test(getCandidate())) {
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }

  @Override
  public NumberExpectation<N> lessThan(N target) {

    if (!NumberPredicates.lessThan(target).test(getCandidate())) {
      String message = "'" + getCandidate() + "' should be less than '" + target + "', but is not.";
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }

  @Override
  public NumberExpectation<N> lessThan(N target, String message) {

    if (!NumberPredicates.lessThan(target).test(getCandidate())) {
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }

  @Override
  public NumberExpectation<N> lessThanOrEqualTo(N target) {

    if (!NumberPredicates.lessThanOrEqualTo(target).test(getCandidate())) {
      String message = "'" + getCandidate() + "' should be less than or equal to '" + target + "', but is not.";
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }

  @Override
  public NumberExpectation<N> lessThanOrEqualTo(N target, String message) {

    if (!NumberPredicates.lessThanOrEqualTo(target).test(getCandidate())) {
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }
}
