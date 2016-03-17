package org.raistlic.common.expectation;

import org.raistlic.common.precondition.InvalidParameterException;

import java.util.function.Function;

/**
 * @author Lei Chen (2015-12-29)
 */
public class Expectations {

  public static ExpectedCases with(Function<String, ? extends RuntimeException> exceptionProvider) {

    if (exceptionProvider == null) {
      throw new InvalidParameterException("'exceptionProvider' is null.");
    }
    return new DefaultExpectedCases(exceptionProvider);
  }

  private Expectations() { }
}
