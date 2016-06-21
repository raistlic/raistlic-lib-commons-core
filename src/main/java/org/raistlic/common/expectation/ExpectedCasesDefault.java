package org.raistlic.common.expectation;

import java.util.Collection;
import java.util.function.Function;

/**
 * The default implementation for {@link ExpectedCases} .
 *
 * @author Lei Chen (2015-12-29)
 */
final class ExpectedCasesDefault implements ExpectedCases {

  private final Function<String, ? extends RuntimeException> exceptionMapper;

  ExpectedCasesDefault(Function<String, ? extends RuntimeException> exceptionMapper) {

    this.exceptionMapper = exceptionMapper;
  }

  @Override
  public BooleanExpectation expect(Boolean value) {

    return new BooleanExpectationDefault(value, exceptionMapper);
  }

  @Override
  public PrimitiveBooleanExpectation expect(boolean value) {

    return new PrimitiveBooleanExpectationDefault(value, exceptionMapper);
  }

  @Override
  public StringExpectation expect(String value) {

    return new StringExpectationDefault(value, exceptionMapper);
  }

  @Override
  public <E> GenericExpectation<E> expect(E value) {

    return new GenericExpectationDefault<>(value, exceptionMapper);
  }

  @Override
  public <E> CollectionExpectation<E> expect(Collection<E> collection) {

    return new CollectionExpectation<>(collection, exceptionMapper);
  }

  @Override
  public <N extends Number & Comparable<N>> NumberExpectation<N> expect(N value) {

    return new NumberExpectationDefault<>(value, exceptionMapper);
  }

  @Override
  public ThreadExpectation expect(Thread thread) {

    return new ThreadExpectationDefault(thread, exceptionMapper);
  }

  @Override
  public void assertThat(boolean assertion, String message) {

    if (!assertion) {
      throw exceptionMapper.apply(message);
    }
  }
}
