package org.raistlic.common.precondition;

import java.util.function.Function;
import java.util.function.Predicate;

/**
 * @author Lei Chen (2015-10-14)
 */
@SuppressWarnings("unchecked")
class AbstractExpectation<C> {

  private final C candidate;

  private Function<String, ? extends RuntimeException> exceptionProvider;

  private Predicate<? super C> predicate;

  private String message;

  AbstractExpectation(C candidate, Function<String, ? extends RuntimeException> exceptionProvider) {

    assert exceptionProvider != null;

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
    assert exceptionProvider != null;

    if (!predicate.test(candidate)) {
      throw exceptionProvider.apply(message);
    }
  }
}
