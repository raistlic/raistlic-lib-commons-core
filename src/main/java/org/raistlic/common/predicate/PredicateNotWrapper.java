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

import org.raistlic.common.precondition.Precondition;

import java.util.function.Predicate;

/**
 * A wrapper class that returns the inverse result of the wrapped original {@link Predicate} .
 *
 * @author Lei CHEN
 * @since 1.3
 */
public final class PredicateNotWrapper<E> implements Predicate<E> {

  private final Predicate<? super E> original;

  /**
   * Wraps the {@code original} {@link Predicate} instance to create the wrapper object.
   *
   * @param original the original {@link Predicate} to be wrapped, cannot be {@code null}.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code original} is
   *         {@code null}.
   */
  public PredicateNotWrapper(Predicate<? super E> original) {

    Precondition.param(original, "original").notNull();

    this.original = original;
  }

  @Override
  public boolean test(E e) {

    return !original.test(e);
  }
}
