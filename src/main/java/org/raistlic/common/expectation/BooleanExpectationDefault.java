package org.raistlic.common.expectation;

import org.raistlic.common.precondition.InvalidParameterException;

import java.util.function.Function;

final class BooleanExpectationDefault extends AbstractGenericExpectation<Boolean, BooleanExpectation>
    implements BooleanExpectation {

  private final Boolean candidate;

  private final Function<String, ? extends RuntimeException> exceptionMapper;

  BooleanExpectationDefault(Boolean candidate, Function<String, ? extends RuntimeException> exceptionMapper) {

    if (exceptionMapper == null) {
      throw new InvalidParameterException("exceptionMapper cannot be null.");
    }

    this.candidate = candidate;
    this.exceptionMapper = exceptionMapper;
  }

  @Override
  BooleanExpectation getThis() {

    return this;
  }

  @Override
  Boolean getCandidate() {

    return candidate;
  }

  @Override
  Function<String, ? extends RuntimeException> getExceptionMapper() {

    return exceptionMapper;
  }

  @Override
  public void isTrue() {

    if (!Boolean.TRUE.equals(candidate)) {
      throw exceptionMapper.apply("Candidate should be true, but was " + candidate);
    }
  }

  @Override
  public void isTrue(String message) {

    if (!Boolean.TRUE.equals(candidate)) {
      throw exceptionMapper.apply(message);
    }
  }

  @Override
  public void isFalse() {

    if (!Boolean.FALSE.equals(candidate)) {
      throw exceptionMapper.apply("Candidate should be false, but was " + candidate);
    }
  }

  @Override
  public void isFalse(String message) {

    if (!Boolean.FALSE.equals(candidate)) {
      throw exceptionMapper.apply(message);
    }
  }
}
