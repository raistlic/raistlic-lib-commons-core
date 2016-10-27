package org.raistlic.common.postcondition;

import org.raistlic.common.assertion.AssertionFactory;
import org.raistlic.common.assertion.AssertionFactoryManager;
import org.raistlic.common.assertion.Assertions;
import org.raistlic.common.assertion.BooleanAssertion;
import org.raistlic.common.assertion.CollectionAssertion;
import org.raistlic.common.assertion.GenericAssertion;
import org.raistlic.common.assertion.NumberAssertion;
import org.raistlic.common.assertion.PrimitiveBooleanAssertion;
import org.raistlic.common.assertion.StringAssertion;

import java.util.Collection;
import java.util.function.Function;

/**
 * The class is used as the entry point for post condition checks, it has utility methods for
 * validation work, as well as some static factory methods that expose proper {@link AssertionFactory}
 * instances for different types of objects.
 *
 * @author Lei Chen (2016-03-17)
 */
public final class Postcondition {

  public static void setExceptionMapper(Function<String, ? extends RuntimeException> exceptionMapper) {

    assertionFactoryManager.setExceptionMapper(exceptionMapper);
  }

  public static void setExpectedCasesStrategy(AssertionFactory.Strategy strategy) {

    assertionFactoryManager.setStrategy(strategy);
  }

  public static void switchOn() {

    assertionFactoryManager.switchOn();
  }

  public static void switchOff() {

    assertionFactoryManager.switchOff();
  }

  public static <E> GenericAssertion<E> assertThat(E entity) {

    return getAssertionFactory().expect(entity);
  }

  public static StringAssertion assertThat(String entity) {

    return getAssertionFactory().expect(entity);
  }

  public static <N extends Number & Comparable<N>> NumberAssertion<N> assertThat(N entity) {

    return getAssertionFactory().expect(entity);
  }

  public static BooleanAssertion assertThat(Boolean entity) {

    return getAssertionFactory().expect(entity);
  }

  public static PrimitiveBooleanAssertion assertThat(boolean entity) {

    return getAssertionFactory().expect(entity);
  }

  public static <E> CollectionAssertion<E> assertThat(Collection<E> entity) {

    return getAssertionFactory().expect(entity);
  }

  public static void isTrue(boolean statement) {

    isTrue(statement, "");
  }

  public static void isTrue(boolean statement, String message) {

    getAssertionFactory().assertThat(statement, message);
  }

  /*
   * Not to be instantiated or inherited.
   */
  private Postcondition() { }

  private static AssertionFactory getAssertionFactory() {

    return assertionFactoryManager.getCurrentFactory();
  }

  private static final AssertionFactoryManager assertionFactoryManager =
      Assertions.createAssertionFactoryManager(PostconditionException::new);
}
