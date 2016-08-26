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

import org.raistlic.common.precondition.Precondition;
import org.raistlic.common.predicate.CollectionPredicates;
import org.raistlic.common.predicate.Predicates;

import java.util.Collection;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * Expectations to validate {@link Collection} instances.
 *
 * @author Lei Chen (2016-03-17)
 */
final class CollectionExpectationDefault<E> extends GenericExpectationAbstract<Collection<E>, CollectionExpectation<E>>
    implements CollectionExpectation<E> {

  private final Function<String, ? extends RuntimeException> exceptionMapper;

  private Collection<E> candidate;

  CollectionExpectationDefault(Collection<E> candidate, Function<String, ? extends RuntimeException> exceptionMapper) {

    Precondition.assertParam(exceptionMapper != null, "'exceptionMapper' cannot be null.");

    this.candidate = candidate;
    this.exceptionMapper = exceptionMapper;
  }

  @Override
  public CollectionExpectation<E> isEmpty() {

    if (CollectionPredicates.isEmpty().test(getCandidate())) {
      return getThis();
    }
    throw getExceptionMapper().apply("Collection should be empty but is not.");
  }

  @Override
  public CollectionExpectation<E> isEmpty(String message) {

    if (CollectionPredicates.isEmpty().test(getCandidate())) {
      return getThis();
    }
    throw getExceptionMapper().apply(message);
  }

  @Override
  public CollectionExpectation<E> isNullOrEmpty() {

    if (IS_NULL_OR_EMPTY.test(getCandidate())) {
      return getThis();
    }
    throw getExceptionMapper().apply("Collection should be null or empty, but is not.");
  }

  @Override
  public CollectionExpectation<E> isNullOrEmpty(String message) {

    if (IS_NULL_OR_EMPTY.test(getCandidate())) {
      return getThis();
    }
    throw getExceptionMapper().apply(message);
  }

  @Override
  public CollectionExpectation<E> notEmpty() {

    if (CollectionPredicates.notEmpty().test(getCandidate())) {
      return getThis();
    }
    throw getExceptionMapper().apply("Collection should not be empty, but is.");
  }

  @Override
  public CollectionExpectation<E> notEmpty(String message) {

    if (CollectionPredicates.notEmpty().test(getCandidate())) {
      return getThis();
    }
    throw getExceptionMapper().apply(message);
  }

  @Override
  public CollectionExpectation<E> hasSize(int size) {

    Collection<E> c = getCandidate();
    if (c != null && c.size() == size) {
      return getThis();
    }
    throw getExceptionMapper().apply("Collection should have size '" + size + "', but does not.");
  }

  @Override
  public CollectionExpectation<E> hasSize(int size, String message) {

    Collection<E> c = getCandidate();
    if (c != null && c.size() == size) {
      return getThis();
    }
    throw getExceptionMapper().apply(message);
  }

  @Override
  public CollectionExpectation<E> contains(E element) {

    Collection<E> c = getCandidate();
    if (c != null && c.contains(element)) {
      return getThis();
    }
    throw getExceptionMapper().apply("Collection should contain '" + element + "', but does not.");
  }

  @Override
  public CollectionExpectation<E> contains(E element, String message) {

    Collection<E> c = getCandidate();
    if (c != null && c.contains(element)) {
      return getThis();
    }
    throw getExceptionMapper().apply(message);
  }

  @Override
  public CollectionExpectation<E> containsAll(Collection<E> elements) {

    Precondition.assertParam(elements != null, "'elements' can not be null.");
    elements.forEach(this::contains);
    return getThis();
  }

  @Override
  public CollectionExpectation<E> containsAll(Collection<E> elements, String message) {

    Precondition.assertParam(elements != null, "'elements' can not be null.");
    elements.forEach(e -> this.contains(e, message));
    return getThis();
  }

  @Override
  CollectionExpectationDefault<E> getThis() {

    return getThis();
  }

  @Override
  Collection<E> getCandidate() {

    return candidate;
  }

  CollectionExpectation<E> setCandidate(Collection<E> candidate) {

    this.candidate = candidate;
    return getThis();
  }

  @Override
  Function<String, ? extends RuntimeException> getExceptionMapper() {

    return exceptionMapper;
  }

  private final Predicate<Collection<?>> IS_NULL_OR_EMPTY = Predicates.or(
      Predicates.isNull(),
      CollectionPredicates.isEmpty()
  );
}
