package org.raistlic.common.expectation;

import java.util.Collection;

/**
 * A collection of different cases to create expectations for different candidates to be checked.
 *
 * @author Lei Chen (2015-12-29)
 */
public interface ExpectedCases {

  BooleanExpectation expect(Boolean candidate);

  PrimitiveBooleanExpectation expect(boolean candidate);

  StringExpectation expect(String candidate);

  <V> GenericExpectation<V> expect(V candidate);

  <E> CollectionExpectation<E> expect(Collection<E> candidate);

  <N extends Number & Comparable<N>> NumberExpectation<N> expect(N candidate);

  ThreadExpectation expect(Thread thread);

  void assertThat(boolean assertion, String message);
}
