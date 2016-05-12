package org.raistlic.common.expectation;

import org.raistlic.common.precondition.Precondition;

import java.util.function.Function;

final class GenericExpectationDefault<E> extends AbstractGenericExpectation<E, GenericExpectation<E>>
    implements GenericExpectation<E> {

  private final E candidate;

  private final Function<String, ? extends RuntimeException> exceptionProvider;

  GenericExpectationDefault(E candidate, Function<String, ? extends RuntimeException> exceptionProvider) {

    Precondition.assertParam(exceptionProvider != null, "'exceptionProvider' should not be null, but it is.");

    this.candidate = candidate;
    this.exceptionProvider = exceptionProvider;
  }

  @Override
  GenericExpectation<E> getThis() {

    return this;
  }

  @Override
  E getCandidate() {

    return candidate;
  }

  @Override
  Function<String, ? extends RuntimeException> getExceptionMapper() {

    return exceptionProvider;
  }
}
