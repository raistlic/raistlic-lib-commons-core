package org.raistlic.common.expectation;

import org.raistlic.common.precondition.InvalidParameterException;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Function;

/**
 * Static factory methods holder, the entry point of the package.
 *
 * @author Lei Chen (2015-12-29)
 */
public class Expectations {

  /**
   * Creates and returns a new instance of {@link ExpectedCases} with the specified exception provider.
   *
   * @param exceptionProvider the exception provider to use, cannot be {@code null}.
   * @return the created {@link ExpectedCases} instance.
   *
   * @throws InvalidParameterException when {@code exceptionProvider} is {@code null}.
   */
  public static ExpectedCases createDefaultExpectedCases(Function<String, ? extends RuntimeException> exceptionProvider) {

    if (exceptionProvider == null) {
      throw new InvalidParameterException("'exceptionProvider' is null.");
    }
    return new ExpectedCasesDefault(exceptionProvider);
  }

  public static ExpectedCases createThreadLocalExpectedCases(Function<String, ? extends RuntimeException> exceptionProvider) {

    throw new UnsupportedOperationException();
  }

  public static ExpectedCases createSwitchableProxy(ExpectedCases expectedCases, AtomicBoolean theSwitch) {

    throw new UnsupportedOperationException();
  }

  /*
   * Static method holder, not to be instanticated or inherited.
   */
  private Expectations() { }
}
