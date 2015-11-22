package org.raistlic.common.precondition;

import java.util.function.Function;
import java.util.function.Predicate;

/**
 * Provides some common logic implementation for the expectation classes.
 *
 * @author Lei Chen (2015-10-14)
 */
@SuppressWarnings("unchecked")
class AbstractExpectation<C> {

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

  void evaluate() {

    assert predicate != null;

    if (!predicate.test(candidate)) {
      throw exceptionProvider.apply(message);
    }
  }
}
