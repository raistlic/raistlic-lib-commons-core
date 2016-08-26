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
import java.util.Comparator;

/**
 * Defines useful expectations around {@link Collection} candidates.
 *
 * @param <E> the actual element type of the {@link Collection} candidate.
 */
public interface CollectionExpectation<E> extends Expectation<Collection<E>, CollectionExpectation<E>> {

  /**
   * Checks that the collection candidate is empty, or otherwise throws an exception.
   *
   * @return the expectation instance itself, for fluent call.
   */
  CollectionExpectation<E> isEmpty();

  /**
   * Checks that the collection candidate is empty, or otherwise throws an exception with the specified {@code message} .
   *
   * @param message the message to throw exception with, when the check fails.
   * @return the expectation instance itself, for fluent call.
   */
  CollectionExpectation<E> isEmpty(String message);

  /**
   * Checks that the collection is either {@code null} or empty, otherwise throws an exception.
   *
   * @return the expectation instance itself, for fluent call.
   */
  CollectionExpectation<E> isNullOrEmpty();

  /**
   * Checks that the collection is either {@code null} or empty, otherwise throws an exception with the specified
   * {@code message} .
   *
   * @param message the message to throw exception with, when the check fails.
   * @return the expectation instance itself, for fluent call.
   */
  CollectionExpectation<E> isNullOrEmpty(String message);

  /**
   * Checks that the collection is empty, otherwise throws an exception.
   *
   * @return the expectation instance itself, for fluent call.
   */
  CollectionExpectation<E> notEmpty();

  /**
   * Checks that the collection is empty, otherwise throws an exception with the specified {@code message} .
   *
   * @param message the message to throw exception with, when the check fails.
   * @return the expectation instance itself, for fluent call.
   */
  CollectionExpectation<E> notEmpty(String message);

  /**
   * Checks that the collection has the specified {@code size} , or otherwise throws an exception.
   *
   * @param size the size that the collection candidate is expected to have.
   * @return the expectation instance itself, for fluent call.
   */
  CollectionExpectation<E> hasSize(int size);

  /**
   * Checks that the collection has the specified {@code size} , or otherwise throws an exception with the specified
   * {@code message} .
   *
   * @param size the size that the collection candidate is expected to have.
   * @param message the message to throw exception with, when the check fails.
   * @return the expectation instance itself, for fluent call.
   */
  CollectionExpectation<E> hasSize(int size, String message);

  /**
   * Checks that the collection contains the specified {@code element} , or otherwise throws an exception.
   *
   * @param element the element that the collection candidate is expected to contain.
   * @return the expectation instance itself, for fluent call.
   */
  CollectionExpectation<E> contains(E element);

  /**
   * Checks that the collection contains the specified {@code element} , or otherwise throws an exception with the
   * specified {@code message} .
   *
   * @param element the element that the collection candidate is expected to contain.
   * @param message the message to throw exception with, when the check fails.
   * @return the expectation instance itself, for fluent call.
   */
  CollectionExpectation<E> contains(E element, String message);

  /**
   * Checks that the collection contains all of the specified elements, or otherwise throws an exception.
   *
   * @param elements the elements that the collection candidate is expected to contain, cannot be {@code null}.
   * @return the expectation instance itself, for fluent call.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code elements} is {@code null}.
   */
  CollectionExpectation<E> containsAll(Collection<E> elements);

  /**
   * Checks that the collection contains all of the specified elements, or otherwise throws an exception with the
   * specified {@code message} .
   *
   * @param elements the elements that the collection candidate is expected to contain, cannot be {@code null}.
   * @param message the message to throw exception with, when the check fails.
   * @return the expectation instance itself, for fluent call.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code elements} is {@code null}.
   */
  CollectionExpectation<E> containsAll(Collection<E> elements, String message);

  /**
   * Checks that the collection is sorted by the specified {@code comparator} , or otherwise throws an exception with the
   * specified {@code message} .
   *
   * @param comparator the comparator that the collection candidate is expected to be sorted by, cannot be {@code null}.
   * @param message the message to throw exception with, when the check fails.
   * @return the expectation instance itself, for fluent call.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code comparator} is {@code null}.
   */
  CollectionExpectation<E> isOrderedBy(Comparator<? super E> comparator, String message);
}
