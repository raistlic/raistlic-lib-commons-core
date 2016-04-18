package org.raistlic.common.expectation;

import org.raistlic.common.predicate.CollectionPredicates;

import java.util.Collection;
import java.util.function.Function;

/**
 * Expectations to validate {@link Collection} instances.
 *
 * @author Lei Chen (2016-03-17)
 */
public class CollectionExpectation<E> extends AbstractGenericExpectation<Collection<E>, CollectionExpectation<E>> {

  CollectionExpectation(Collection<E> candidate,
                        String name,
                        Function<String, ? extends RuntimeException> exceptionProvider) {

    super(candidate, name, exceptionProvider);
  }

  public CollectionExpectation<E> isEmpty() {

    String message = "";
    if (name() != null) {
      message = "'" + name() + "' should be empty but is not: " + getCandidate();
    }
    return isEmpty(message);
  }

  public CollectionExpectation<E> isEmpty(String message) {

    setMessage(message);
    setPredicate(CollectionPredicates.isEmpty());
    return evaluate();
  }

  public CollectionExpectation<E> isNullOrEmpty() {

    if (getCandidate() == null) {
      return getThis();
    }
    String message = "Collection " + candidateForMessage() + "should be null or empty, but has size: " +
            getCandidate().size();
    return isNullOrEmpty(message);
  }

  public CollectionExpectation<E> isNullOrEmpty(String message) {

    if (getCandidate() == null) {
      return getThis();
    }
    setMessage(message);
    setPredicate(CollectionPredicates.isEmpty());
    return evaluate();
  }

  public CollectionExpectation<E> notEmpty() {

    String message = "Collection " + candidateForMessage() + "should be not empty, but " +
            (getCandidate() == null ? " is null" : " it is");
    return notEmpty(message);
  }

  public CollectionExpectation<E> notEmpty(String message) {

    isNotNull(message);
    setMessage(message);
    setPredicate(CollectionPredicates.notEmpty());
    return evaluate();
  }

  public CollectionExpectation<E> hasSize(int size) {

    String message = "Collection " + candidateForMessage() + "should have size " + size + ", " +
            ((getCandidate() == null) ? "but is null" : "but size is " + getCandidate().size());
    return hasSize(size, message);
  }

  public CollectionExpectation<E> hasSize(int size, String message) {

    setMessage(message);
    setPredicate(CollectionPredicates.hasSize(size));
    return evaluate();
  }

  public CollectionExpectation<E> contains(E element) {

    String message = "Collection " + candidateForMessage() + "should contains '" + element + "' but not.";
    return contains(element, message);
  }

  public CollectionExpectation<E> contains(E element, String message) {

    setMessage(message);
    setPredicate(CollectionPredicates.contains(element));
    return evaluate();
  }
}
