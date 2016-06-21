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
public class CollectionExpectation<E> extends GenericExpectationAbstract<Collection<E>, CollectionExpectation<E>> {

  private final Collection<E> candidate;

  private final Function<String, ? extends RuntimeException> exceptionMapper;

  CollectionExpectation(Collection<E> candidate, Function<String, ? extends RuntimeException> exceptionMapper) {

    Precondition.assertParam(exceptionMapper != null, "'exceptionMapper' cannot be null.");

    this.candidate = candidate;
    this.exceptionMapper = exceptionMapper;
  }

  public CollectionExpectation<E> isEmpty() {

    if (CollectionPredicates.isEmpty().test(getCandidate())) {
      return getThis();
    }
    throw getExceptionMapper().apply("Collection should be empty but is not.");
  }

  public CollectionExpectation<E> isEmpty(String message) {

    if (CollectionPredicates.isEmpty().test(getCandidate())) {
      return getThis();
    }
    throw getExceptionMapper().apply(message);
  }

  public CollectionExpectation<E> isNullOrEmpty() {

    if (IS_NULL_OR_EMPTY.test(getCandidate())) {
      return getThis();
    }
    throw getExceptionMapper().apply("Collection should be null or empty, but is not.");
  }

  public CollectionExpectation<E> isNullOrEmpty(String message) {

    if (IS_NULL_OR_EMPTY.test(getCandidate())) {
      return getThis();
    }
    throw getExceptionMapper().apply(message);
  }

  public CollectionExpectation<E> notEmpty() {

    if (CollectionPredicates.notEmpty().test(getCandidate())) {
      return getThis();
    }
    throw getExceptionMapper().apply("Collection should not be empty, but is.");
  }

  public CollectionExpectation<E> notEmpty(String message) {

    if (CollectionPredicates.notEmpty().test(getCandidate())) {
      return getThis();
    }
    throw getExceptionMapper().apply(message);
  }

  public CollectionExpectation<E> hasSize(int size) {

    Collection<E> c = getCandidate();
    if (c != null && c.size() == size) {
      return getThis();
    }
    throw getExceptionMapper().apply("Collection should have size '" + size + "', but does not.");
  }

  public CollectionExpectation<E> hasSize(int size, String message) {

    Collection<E> c = getCandidate();
    if (c != null && c.size() == size) {
      return getThis();
    }
    throw getExceptionMapper().apply(message);
  }

  public CollectionExpectation<E> contains(E element) {

    Collection<E> c = getCandidate();
    if (c != null && c.contains(element)) {
      return getThis();
    }
    throw getExceptionMapper().apply("Collection should contain '" + element + "', but does not.");
  }

  public CollectionExpectation<E> contains(E element, String message) {

    Collection<E> c = getCandidate();
    if (c != null && c.contains(element)) {
      return getThis();
    }
    throw getExceptionMapper().apply(message);
  }

  public CollectionExpectation<E> containsAll(Collection<E> elements) {

    Precondition.assertParam(elements != null, "'elements' can not be null.");
    elements.forEach(this::contains);
    return this;
  }

  public CollectionExpectation<E> containsAll(Collection<E> elements, String message) {

    Precondition.assertParam(elements != null, "'elements' can not be null.");
    elements.forEach(e -> this.contains(e, message));
    return this;
  }

  @Override
  CollectionExpectation<E> getThis() {

    return this;
  }

  @Override
  Collection<E> getCandidate() {

    return candidate;
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
