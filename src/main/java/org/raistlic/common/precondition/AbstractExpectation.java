package org.raistlic.common.precondition;

import java.util.function.Predicate;

/**
 * @author Lei Chen (2015-10-14)
 */
@SuppressWarnings("unchecked")
class AbstractExpectation<C, E extends AbstractExpectation<C, E>> {

  private final C candidate;

  private ExceptionBuilder<?> exceptionFactory;

  private Predicate<? super C> predicate;

  private String message;

  AbstractExpectation(C candidate, ExceptionBuilder<?> exceptionFactory) {

    this.candidate = candidate;
    this.exceptionFactory = exceptionFactory;
  }

  C getCandidate() {

    return candidate;
  }

  E withPredicate(Predicate<? super C> predicate) {

    assert predicate != null;

    this.predicate = predicate;
    return (E) this;
  }

  E withMessage(String message) {

    this.message = message;
    return (E) this;
  }

  E evaluate() {

    assert candidate != null;
    assert predicate != null;
    assert exceptionFactory != null;

    if (!predicate.test(candidate)) {
      throw exceptionFactory.withMessage(message).withCause(null).build();
    }
    return (E) this;
  }
}
