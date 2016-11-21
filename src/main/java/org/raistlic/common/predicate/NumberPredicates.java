package org.raistlic.common.predicate;

import org.raistlic.common.precondition.Precondition;

import java.util.function.Predicate;

/**
 * The class holds some static factory methods that export different types of {@code Predicate}
 * instances for testing {@link Number} s.
 *
 * @author Lei Chen (2015-10-16)
 */
public final class NumberPredicates {

  /**
   * The method exports a {@link Predicate} instance for testing number values, that are greater
   * than the specified {@code reference} .
   *
   * @param <N> the actual number type.
   * @param reference the value used to create the {@link Predicate} and to test integers.
   * @return the {@link Predicate} instance created.
   */
  public static <N extends Number & Comparable<N>> Predicate<N> greaterThan(N reference) {

    Precondition.assertParam(reference != null, "greaterThan(reference): reference cannot be null.");

    return new NumberGreaterThanPredicate<N>(reference);
  }

  /**
   * The method exports a {@link Predicate} instance for testing number values, that are greater
   * than or equal to the specified {@code reference} .
   *
   * @param <N> the actual number type.
   * @param reference the reference used to create the {@link Predicate} and to test integers.
   * @return the {@link Predicate} instance created.
   */
  public static <N extends Number & Comparable<N>> Predicate<N> greaterThanOrEqualTo(N reference) {

    Precondition.assertParam(reference != null, "greaterThanOrEqualTo(reference): reference cannot be null.");

    return Predicates.or(Predicates.equalTo(reference), greaterThan(reference));
  }

  /**
   * The method exports a {@link Predicate} instance for testing number values, that are less than
   * the specified {@code reference} .
   *
   * @param <N> the actual number type.
   * @param reference the reference used to create the {@link Predicate} and to test integers.
   * @return the {@link Predicate} instance created.
   */
  public static <N extends Number & Comparable<N>> Predicate<N> lessThan(N reference) {

    Precondition.assertParam(reference != null, "lessThan(reference): reference cannot be null.");

    return new NumberLessThanPredicate<N>(reference);
  }

  /**
   * The method exports a {@link Predicate} instance for testing number values, that are less than
   * or equal to the specified {@code reference} .
   *
   * @param <N> the actual number type.
   * @param reference the reference used to create the {@link Predicate} and to test integers.
   * @return the {@link Predicate} instance created.
   */
  public static <N extends Number & Comparable<N>> Predicate<N> lessThanOrEqualTo(N reference) {

    Precondition.assertParam(reference != null, "lessThanOrEqualTo(reference): reference cannot be null.");

    return Predicates.or(Predicates.equalTo(reference), lessThan(reference));
  }

  private static final class NumberGreaterThanPredicate<N extends Number & Comparable<N>>
      implements Predicate<N> {

    private final N reference;

    public NumberGreaterThanPredicate(N reference) {

      this.reference = reference;
    }

    @Override
    public boolean test(N number) {

      return number != null && number.compareTo(reference) > 0;
    }
  }

  private static final class NumberLessThanPredicate<N extends Number & Comparable<N>>
      implements Predicate<N> {

    private final N reference;

    public NumberLessThanPredicate(N reference) {

      this.reference = reference;
    }

    @Override
    public boolean test(N number) {

      return number != null && number.compareTo(reference) < 0;
    }
  }

  /*
   * Not to be instantiated or inherited.
   */
  private NumberPredicates() { }
}
