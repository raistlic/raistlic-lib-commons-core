package org.raistlic.common.expectation;

import java.util.Collection;

/**
 * A collection of different cases to create expectations for different candidates to be checked.
 *
 * @author Lei Chen (2015-12-29)
 */
public interface ExpectedCases {

  <V> GenericExpectation<V> expect(V value);

  <V> GenericExpectation<V> expect(V value, String name);

  StringExpectation expect(String value);

  StringExpectation expect(String value, String name);

  <E> CollectionExpectation<E> expect(Collection<E> collection);

  <E> CollectionExpectation<E> expect(Collection<E> collection, String name);

  <N extends Number & Comparable<N>> NumberExpectation<N> expect(N parameter);

  <N extends Number & Comparable<N>> NumberExpectation<N> expect(N parameter, String name);

  BooleanExpectation.Boxed expect(Boolean parameter);

  BooleanExpectation.Boxed expect(Boolean parameter, String name);

  BooleanExpectation.Primitive expect(boolean parameter);

  BooleanExpectation.Primitive expect(boolean parameter, String name);

  void assertThat(boolean assertion, String message);
}
