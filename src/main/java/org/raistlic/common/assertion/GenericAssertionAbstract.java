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

package org.raistlic.common.assertion;

import org.raistlic.common.precondition.Precondition;
import org.raistlic.common.predicate.Predicates;

import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * The class helps to check a specified {@code candidate} with certain expectations, and throws
 * a custom runtime exception when the check fails.
 */
public abstract class GenericAssertionAbstract<C, E extends Assertion<C, E>> implements Assertion<C, E> {

  abstract E getThis();

  abstract C getCandidate();

  abstract Function<String, ? extends RuntimeException> getExceptionMapper();

  /**
   * The method claims that the {@code candidate} should be {@code null}, otherwise a runtime
   * exception will be thrown.
   *
   * @return the expectation instance itself, for method calling chain.
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *                                    where the expectation is used.
   */
  @Override
  public E isNull() {

    return isNull("Candidate should be null, but it is " + getCandidate());
  }

  /**
   * The method claims that the {@code candidate} should be {@code null}, otherwise a runtime
   * exception with the specified {@code message} will be thrown.
   *
   * @param message the message to be thrown with the exception, in case the check fails.
   * @return the expectation instance itself, for method calling chain.
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *                                    where the expectation is used.
   */
  public E isNull(String message) {

    if (getCandidate() != null) {
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }

  /**
   * The method claims that the {@code candidate} should not be {@code null}, otherwise a runtime
   * exception will be thrown.
   *
   * @return the expectation instance itself, for method calling chain.
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *                                    where the expectation is used.
   */
  public E isNotNull() {

    return isNotNull("Candidate should not be null, but it is.");
  }

  /**
   * The method claims that the {@code candidate} should not be {@code null}, otherwise a runtime
   * exception with the specified {@code message} will be thrown.
   *
   * @param message the message to be thrown with the exception, in case the check fails.
   * @return the expectation instance itself, for method calling chain.
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *                                    where the expectation is used.
   */
  public E isNotNull(String message) {

    if (getCandidate() == null) {
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }

  /**
   * The method claims that the {@code candidate} should be equal to the {@code target}, otherwise a
   * runtime exception will be thrown.
   *
   * @param target the reference target which the candidate should be equal to, or {@code null} if
   *               the candidate should be {@code null}.
   * @return the expectation instance itself, for method calling chain.
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *                                    where the expectation is used.
   */
  public E isEqualTo(C target) {

    if (!Objects.equals(getCandidate(), target)) {
      String message = "'" + getCandidate() + "' and '" + target + "' is not equal.";
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }

  /**
   * The method claims that the {@code candidate} should be equal to the {@code target}, otherwise a
   * runtime exception with the specified {@code message} will be thrown.
   *
   * @param target  the reference target which the candidate should be equal to, or {@code null} if
   *                the candidate should be {@code null}.
   * @param message the message to be thrown with the exception, in case the check fails.
   * @return the expectation instance itself, for method calling chain.
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *                                    where the expectation is used.
   */
  public E isEqualTo(C target, String message) {

    if (!Objects.equals(getCandidate(), target)) {
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }

  /**
   * The method claims that the {@code candidate} should NOT be equal to the {@code target},
   * otherwise a runtime exception will be thrown.
   *
   * @param target the reference target which the candidate should be equal to, or {@code null} if
   *               the candidate should be {@code null}.
   * @return the expectation instance itself, for method calling chain.
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *                                    where the expectation is used.
   */
  public E isNotEqualTo(C target) {

    if (Objects.equals(getCandidate(), target)) {
      String message = "'" + getCandidate() + "' and '" + target + "' are (unexpectedly) equal.";
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }

  /**
   * The method claims that the {@code candidate} should NOT be equal to the {@code target},
   * otherwise a runtime exception with the specified {@code message} will be thrown.
   *
   * @param target  the reference target which the candidate should be equal to, or {@code null} if
   *                the candidate should be {@code null}.
   * @param message the message to be thrown with the exception, in case the check fails.
   * @return the expectation instance itself, for method calling chain.
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *                                    where the expectation is used.
   */
  public E isNotEqualTo(C target, String message) {

    if (Objects.equals(getCandidate(), target)) {
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }

  /**
   * The method claims that the {@code candidate} should be an instance of the specified {@code type} ,
   * otherwise a runtime exception will be thrown.
   *
   * @param type the type of which the {@code candidate} claims to be.
   * @return the expectation instance itself, for method calling chain.
   */
  public E isInstanceOf(Class<?> type) {

    Precondition.assertParam(type != null, "'type' should not be null, but it is.");

    if (!Predicates.instanceOf(type).test(getCandidate())) {
      String message = "'" + getCandidate() + "' should be instance of type '" + type + "', but is " +
        (getCandidate() == null ? "null" : "not (actual type: '" + getCandidate().getClass().getName() + "')");
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }

  /**
   * The method claims that the {@code candidate} should be an instance of the specified {@code type} ,
   * otherwise a runtime exception with the specified {@code message} will be thrown.
   *
   * @param type    the type of which the {@code candidate} claims to be.
   * @param message the message to be thrown with the exception, in case the check fails.
   * @return the expectation instance itself, for method calling chain.
   */
  public E isInstanceOf(Class<?> type, String message) {

    Precondition.assertParam(type != null, "'type' should not be null, but it is.");

    if (!Predicates.instanceOf(type).test(getCandidate())) {
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }

  /**
   * The methods claims that the {@code candidate} should match the specified {@code predicate} ,
   * otherwise a runtime exception will be thrown.
   *
   * @param predicate the predicate that's used to test the {@code candidate} , cannot be {@code null}.
   * @return the expectation instance itself, for method calling chain.
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *                                    where the expectation is used.
   */
  public E matches(Predicate<? super C> predicate) {

    Precondition.assertParam(predicate != null, "'predicate' should not be null, but it is.");

    if (!predicate.test(getCandidate())) {
      String message = "'" + getCandidate() + "' does not match the specified predicate: '" + predicate + "'";
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }

  /**
   * The methods claims that the {@code candidate} should match the specified {@code predicate} ,
   * otherwise a runtime exception with the {@code message} will be thrown.
   *
   * @param predicate the predicate that's used to test the {@code candidate} , cannot be {@code null}.
   * @param message   the message to be thrown with the exception, in case the check fails.
   * @return the expectation instance itself, for method calling chain.
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *                                    where the expectation is used.
   */
  public E matches(Predicate<? super C> predicate, String message) {

    Precondition.assertParam(predicate != null, "'predicate' should not be null, but it is.");

    if (!predicate.test(getCandidate())) {
      throw getExceptionMapper().apply(message);
    }
    return getThis();
  }
}
