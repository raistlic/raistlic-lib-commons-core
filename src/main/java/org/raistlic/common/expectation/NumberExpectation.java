package org.raistlic.common.expectation;

public interface NumberExpectation<N extends Number & Comparable<N>> extends Expectation<N, NumberExpectation<N>> {

  NumberExpectation<N> greaterThan(N target);

  NumberExpectation<N> greaterThan(N target, String message);

  NumberExpectation<N> greaterThanOrEqualTo(N target);

  NumberExpectation<N> greaterThanOrEqualTo(N target, String message);

  NumberExpectation<N> lessThan(N target);

  NumberExpectation<N> lessThan(N target, String message);

  NumberExpectation<N> lessThanOrEqualTo(N target);

  NumberExpectation<N> lessThanOrEqualTo(N target, String message);
}
