package org.raistlic.common.expectation;

import org.raistlic.common.predicate.CollectionPredicates;

import java.util.Collection;
import java.util.function.Function;

/**
 * Expectations to validate {@link Collection} instances.
 *
 * @author Lei Chen (2016-03-17)
 */
public class CollectionExpectation<E, C extends Collection<E>> extends GeneralExpectation<C> {

  CollectionExpectation(C candidate,
                        String name,
                        Function<String, ? extends RuntimeException> exceptionProvider) {

    super(candidate, name, exceptionProvider);
  }

  public void isEmpty() {

    String message = "";
    if (name() != null) {
      message = "'" + name() + "' should be empty but is not: " + getCandidate();
    }
    isEmpty(message);
  }

  public void isEmpty(String message) {

    setMessage(message);
    setPredicate(CollectionPredicates.isEmpty());
    evaluate();
  }

  public void isNullOrEmpty() {

    if (getCandidate() == null) {
      return;
    }
    String message = "Collection " + nameForMessgae() + "should be null or empty, but has size: " +
            getCandidate().size();
    isNullOrEmpty(message);
  }

  public void isNullOrEmpty(String message) {

    if (getCandidate() == null) {
      return;
    }
    setMessage(message);
    setPredicate(CollectionPredicates.isEmpty());
    evaluate();
  }

  public void notEmpty() {

    String message = "Collection " + nameForMessgae() + "should be not empty, but " +
            (getCandidate() == null ? " is null" : " it is");
    notEmpty(message);
  }

  public void notEmpty(String message) {

    notNull(message);
    setMessage(message);
    setPredicate(CollectionPredicates.notEmpty());
    evaluate();
  }

  public void hasSize(int size) {

    String message = "Collection " + nameForMessgae() + "should have size " + size + ", " +
            ((getCandidate() == null) ? "but is null" : "but size is " + getCandidate().size());
    hasSize(size, message);
  }

  public void hasSize(int size, String message) {

    setMessage(message);
    setPredicate(CollectionPredicates.hasSize(size));
    evaluate();
  }

  public void contains(E element) {

    String message = "Collection " + nameForMessgae() + "should contains '" + element + "' but not.";
    contains(element, message);
  }

  public void contains(E element, String message) {

    setMessage(message);
    setPredicate(CollectionPredicates.contains(element));
    evaluate();
  }
}
