package org.raistlic.common.precondition;

import java.util.function.Function;

/**
 * @author Lei Chen (2015-12-29)
 */
final class DefaultExpectedCases implements ExpectedCases {

  private final Function<String, ? extends RuntimeException> exceptionProvider;

  DefaultExpectedCases(Function<String, ? extends RuntimeException> exceptionProvider) {

    if (exceptionProvider == null) {
      throw new InvalidParameterException("'exceptionProvider' cannot be null.");
    }
    this.exceptionProvider = exceptionProvider;
  }

  @Override
  public <V> GeneralExpectation<V> expect(V value) {

    return expect(value, null);
  }

  @Override
  public <V> GeneralExpectation<V> expect(V value, String name) {

    return new GeneralExpectation<V>(value, name, exceptionProvider);
  }

  @Override
  public StringExpectation expect(String value) {

    return expect(value, null);
  }

  @Override
  public StringExpectation expect(String value, String name) {

    return new StringExpectation(value, name, exceptionProvider);
  }

  @Override
  public <N extends Number & Comparable<N>> NumberExpectation<N> expect(N value) {

    return expect(value, null);
  }

  @Override
  public <N extends Number & Comparable<N>> NumberExpectation<N> expect(N value, String name) {

    return new NumberExpectation<>(value, name, exceptionProvider);
  }

  @Override
  public BooleanExpectation.Boxed expect(Boolean value) {

    return expect(value, null);
  }

  @Override
  public BooleanExpectation.Boxed expect(Boolean value, String name) {

    return new BooleanExpectation.Boxed(value, name, exceptionProvider);
  }

  @Override
  public BooleanExpectation.Primitive expect(boolean value) {

    return expect(value, null);
  }

  @Override
  public BooleanExpectation.Primitive expect(boolean value, String name) {

    return new BooleanExpectation.Primitive(value, name, exceptionProvider);
  }

  @Override
  public void assertThat(boolean assertion, String message) {

    if (!assertion) {
      throw exceptionProvider.apply(message);
    }
  }
}
