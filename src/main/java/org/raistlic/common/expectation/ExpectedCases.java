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

package org.raistlic.common.expectation;

import java.util.Collection;

/**
 * Export a collection of expectation instances for different types of candidates.
 */
public interface ExpectedCases {

  /**
   * The strategy of managing the expectation instances.
   */
  enum Strategy {

    /**
     * Create a new expectation instance on each expect method call.
     */
    CREATE_NEW,

    /**
     * Reuse a thread local expectation instance on expect method call.
     */
    THREAD_LOCAL
  }

  /**
   * Wraps the specified {@link Boolean} candidate and returns an expectation instance to check it.
   *
   * @param candidate the candidate to be wrapped.
   * @return the expectation instance.
   */
  BooleanExpectation expect(Boolean candidate);

  /**
   * Wraps the specified {@code boolean} candidate and returns an expectation instance to check it.
   *
   * @param candidate the candidate to be wrapped.
   * @return the expectation instance.
   */
  PrimitiveBooleanExpectation expect(boolean candidate);

  /**
   * Wraps the specified {@link String} candidate and returns an expectation instance to check it.
   *
   * @param candidate the candidate to be wrapped.
   * @return the expectation instance.
   */
  StringExpectation expect(String candidate);

  /**
   * Wraps the specified generic candidate and returns an expectation instance to check it.
   *
   * @param candidate the candidate to be wrapped.
   * @return the expectation instance.
   */
  <V> GenericExpectation<V> expect(V candidate);

  /**
   * Wraps the specified {@link Collection} candidate and returns an expectation instance to check it.
   *
   * @param candidate the candidate to be wrapped.
   * @return the expectation instance.
   */
  <E> CollectionExpectation<E> expect(Collection<E> candidate);

  /**
   * Wraps the specified {@link Number} candidate and returns an expectation instance to check it.
   *
   * @param candidate the candidate to be wrapped.
   * @return the expectation instance.
   */
  <N extends Number & Comparable<N>> NumberExpectation<N> expect(N candidate);

  /**
   * Wraps the specified {@link Thread} candidate and returns an expectation instance to check it.
   *
   * @param thread the candidate to be wrapped.
   * @return the expectation instance.
   */
  ThreadExpectation expect(Thread thread);

  /**
   * Makes a claim that the specified {@code assertion} is {@code true}, or otherwise throws an exception with the
   * {@code message} .
   *
   * @param assertion the assertion that's expected to be {@code true}.
   * @param message the message to throw exception with when the {@code assertion} is {@code false}.
   */
  void assertThat(boolean assertion, String message);
}
