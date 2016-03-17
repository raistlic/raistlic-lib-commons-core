package org.raistlic.common.postcondition;

import org.raistlic.common.expectation.BooleanExpectation;
import org.raistlic.common.expectation.CollectionExpectation;
import org.raistlic.common.expectation.Expectations;
import org.raistlic.common.expectation.ExpectedCases;
import org.raistlic.common.expectation.GeneralExpectation;
import org.raistlic.common.expectation.NumberExpectation;
import org.raistlic.common.expectation.StringExpectation;
import org.raistlic.common.precondition.Precondition;

import java.util.Collection;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

/**
 * The class is used as the entry point for post condition checks, it has utility methods for
 * validation work, as well as some static factory methods that expose proper {@link ExpectedCases}
 * instances for different types of objects.
 *
 * @author Lei Chen (2016-03-17)
 */
public final class Postcondition {

  private static final AtomicReference<ExpectedCases> EXPECTED_CASES =
          new AtomicReference<>(Expectations.with(PostconditionException::new));

  public static void setPostconditionExceptionMapper(Function<String, ? extends RuntimeException> exceptionMapper) {

    Precondition.param(exceptionMapper, "exceptionMapper").notNull();
    EXPECTED_CASES.set(Expectations.with(exceptionMapper));
  }

  private static ExpectedCases expectedCases() {

    return EXPECTED_CASES.get();
  }

  public static <E> GeneralExpectation<E> assertThat(E entity) {

    return assertThat(entity, null);
  }

  public static <E> GeneralExpectation<E> assertThat(E entity, String name) {

    return expectedCases().expect(entity, name);
  }

  public static StringExpectation assertThat(String entity) {

    return assertThat(entity, null);
  }

  public static StringExpectation assertThat(String entity, String name) {

    return expectedCases().expect(entity, name);
  }

  public static <N extends Number & Comparable<N>> NumberExpectation<N> assertThat(N entity) {

    return assertThat(entity, null);
  }

  public static <N extends Number & Comparable<N>> NumberExpectation<N> assertThat(N entity, String name) {

    return expectedCases().expect(entity, name);
  }

  public static BooleanExpectation.Boxed assertThat(Boolean entity) {

    return assertThat(entity, null);
  }

  public static BooleanExpectation.Boxed assertThat(Boolean entity, String name) {

    return expectedCases().expect(entity, name);
  }

  public static BooleanExpectation.Primitive assertThat(boolean entity) {

    return assertThat(entity, null);
  }

  public static BooleanExpectation.Primitive assertThat(boolean entity, String name) {

    return expectedCases().expect(entity, name);
  }

  public static <E, C extends Collection<E>> CollectionExpectation<E, C> assertThat(C entity) {

    return assertThat(entity, null);
  }

  public static <E, C extends Collection<E>> CollectionExpectation<E, C> assertThat(C entity, String name) {

    return expectedCases().expect(entity, name);
  }

  public static void isTrue(boolean statement) {

    isTrue(statement, "");
  }

  public static void isTrue(boolean statement, String message) {

    expectedCases().assertThat(statement, message);
  }

  /*
   * Not to be instantiated or inherited.
   */
  private Postcondition() { }
}
