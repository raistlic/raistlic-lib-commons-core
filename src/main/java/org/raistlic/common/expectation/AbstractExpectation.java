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

import java.util.function.Function;
import java.util.function.Predicate;

/**
 * Provides some common logic implementation for the expectation classes.
 *
 * @author Lei Chen (2015-10-14)
 */
@SuppressWarnings("unchecked")
abstract class AbstractExpectation<C, E> {

  /**
   * the candidate to be evaluated.
   */
  private final C candidate;

  /**
   * the exception provider, in case the check fails
   */
  private final Function<String, ? extends RuntimeException> exceptionProvider;

  /**
   * the predicate to check the candidate with
   */
  private Predicate<? super C> predicate;

  /**
   * the message to be thrown with the exception, when the check fails.
   */
  private String message;

  AbstractExpectation(C candidate, Function<String, ? extends RuntimeException> exceptionProvider) {

    if (exceptionProvider == null) {
      throw new InvalidParameterException("'exceptionProvider' should not be null.");
    }

    this.candidate = candidate;
    this.exceptionProvider = exceptionProvider;
  }

  abstract E getThis();

  C getCandidate() {

    return candidate;
  }

  void setPredicate(Predicate<? super C> predicate) {

    assert predicate != null;

    this.predicate = predicate;
  }

  void setMessage(String message) {

    this.message = message;
  }

  E evaluate() {

    assert predicate != null;

    if (!predicate.test(candidate)) {
      throw exceptionProvider.apply(message);
    }
    return getThis();
  }
}
