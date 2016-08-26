package org.raistlic.common.postcondition;

import org.raistlic.common.expectation.BooleanExpectation;
import org.raistlic.common.expectation.CollectionExpectation;
import org.raistlic.common.expectation.Expectations;
import org.raistlic.common.expectation.ExpectedCases;
import org.raistlic.common.expectation.ExpectedCasesFactory;
import org.raistlic.common.expectation.GenericExpectation;
import org.raistlic.common.expectation.NumberExpectation;
import org.raistlic.common.expectation.PrimitiveBooleanExpectation;
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

  public static void setExceptionMapper(Function<String, ? extends RuntimeException> exceptionMapper) {

    Precondition.param(exceptionMapper).isNotNull();

    expectedCasesFactory.setExceptionMapper(exceptionMapper)
        .ifPresent(expectedCasesReference::set);
  }

  public static void setExpectedCasesStrategy(ExpectedCases.Strategy strategy) {

    Precondition.param(strategy).isNotNull();

    expectedCasesFactory.setExpectedCasesStrategy(strategy)
        .ifPresent(expectedCasesReference::set);
  }

  public static void switchOn() {

    expectedCasesFactory.setSwitch(true);
  }

  public static void switchOff() {

    expectedCasesFactory.setSwitch(false);
  }

  private static ExpectedCases expectedCases() {

    return expectedCasesReference.get();
  }

  public static <E> GenericExpectation<E> assertThat(E entity) {

    return expectedCases().expect(entity);
  }

  public static StringExpectation assertThat(String entity) {

    return expectedCases().expect(entity);
  }

  public static <N extends Number & Comparable<N>> NumberExpectation<N> assertThat(N entity) {

    return expectedCases().expect(entity);
  }

  public static BooleanExpectation assertThat(Boolean entity) {

    return expectedCases().expect(entity);
  }

  public static PrimitiveBooleanExpectation assertThat(boolean entity) {

    return expectedCases().expect(entity);
  }

  public static <E> CollectionExpectation<E> assertThat(Collection<E> entity) {

    return expectedCases().expect(entity);
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

  private static final ExpectedCasesFactory expectedCasesFactory = new ExpectedCasesFactory(
      PostconditionException::new, ExpectedCases.Strategy.CREATE_NEW
  );

  private static final AtomicReference<ExpectedCases> expectedCasesReference =
      new AtomicReference<>(Expectations.createDefaultExpectedCases(PostconditionException::new));
}
