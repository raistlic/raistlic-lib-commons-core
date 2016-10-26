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

package org.raistlic.common.assertion;

import java.util.function.Predicate;

/**
 * Defines some generic useful assertions.
 *
 * @param <C> the actual candidate type.
 * @param <E> the actual {@link Assertion} sub-type.
 */
public interface Assertion<C, E extends Assertion<C, E>> {

  /**
   * The method claims that the {@code candidate} should be {@code null}, otherwise a runtime
   * exception will be thrown.
   *
   * @return the expectation instance itself, for method calling chain.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  E isNull();

  /**
   * The method claims that the {@code candidate} should be {@code null}, otherwise a runtime
   * exception with the specified {@code message} will be thrown.
   *
   * @param message the message to be thrown with the exception, in case the check fails.
   * @return the expectation instance itself, for method calling chain.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  E isNull(String message);

  /**
   * The method claims that the {@code candidate} should not be {@code null}, otherwise a runtime
   * exception will be thrown.
   *
   * @return the expectation instance itself, for method calling chain.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  E isNotNull();

  /**
   * The method claims that the {@code candidate} should not be {@code null}, otherwise a runtime
   * exception with the specified {@code message} will be thrown.
   *
   * @param message the message to be thrown with the exception, in case the check fails.
   * @return the expectation instance itself, for method calling chain.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  E isNotNull(String message);

  /**
   * The method claims that the {@code candidate} should be equal to the {@code target}, otherwise a
   * runtime exception will be thrown.
   *
   * @param target the reference target which the candidate should be equal to, or {@code null} if
   *               the candidate should be {@code null}.
   * @return the expectation instance itself, for method calling chain.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  E isEqualTo(C target);

  /**
   * The method claims that the {@code candidate} should be equal to the {@code target}, otherwise a
   * runtime exception with the specified {@code message} will be thrown.
   *
   * @param target  the reference target which the candidate should be equal to, or {@code null} if
   *                the candidate should be {@code null}.
   * @param message the message to be thrown with the exception, in case the check fails.
   * @return the expectation instance itself, for method calling chain.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  E isEqualTo(C target, String message);

  /**
   * The method claims that the {@code candidate} should NOT be equal to the {@code target},
   * otherwise a runtime exception will be thrown.
   *
   * @param target the reference target which the candidate should be equal to, or {@code null} if
   *               the candidate should be {@code null}.
   * @return the expectation instance itself, for method calling chain.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  E isNotEqualTo(C target);

  /**
   * The method claims that the {@code candidate} should NOT be equal to the {@code target},
   * otherwise a runtime exception with the specified {@code message} will be thrown.
   *
   * @param target the reference target which the candidate should be equal to, or {@code null} if
   *               the candidate should be {@code null}.
   * @param message the message to be thrown with the exception, in case the check fails.
   * @return the expectation instance itself, for method calling chain.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  E isNotEqualTo(C target, String message);

  /**
   * The method claims that the {@code candidate} should be an instance of the specified {@code type} ,
   * otherwise a runtime exception will be thrown.
   *
   * @param type the type of which the {@code candidate} claims to be.
   * @return the expectation instance itself, for method calling chain.
   */
  E isInstanceOf(Class<?> type);

  /**
   * The method claims that the {@code candidate} should be an instance of the specified {@code type} ,
   * otherwise a runtime exception with the specified {@code message} will be thrown.
   *
   * @param type the type of which the {@code candidate} claims to be.
   * @param message the message to be thrown with the exception, in case the check fails.
   * @return the expectation instance itself, for method calling chain.
   */
  E isInstanceOf(Class<?> type, String message);

  /**
   * The methods claims that the {@code candidate} should match the specified {@code predicate} ,
   * otherwise a runtime exception will be thrown.
   *
   * @param predicate the predicate that's used to test the {@code candidate} , cannot be {@code null}.
   * @return the expectation instance itself, for method calling chain.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  E matches(Predicate<? super C> predicate);

  /**
   * The methods claims that the {@code candidate} should match the specified {@code predicate} ,
   * otherwise a runtime exception with the {@code message} will be thrown.
   *
   * @param predicate the predicate that's used to test the {@code candidate} , cannot be {@code null}.
   * @param message the message to be thrown with the exception, in case the check fails.
   * @return the expectation instance itself, for method calling chain.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  E matches(Predicate<? super C> predicate, String message);
}
