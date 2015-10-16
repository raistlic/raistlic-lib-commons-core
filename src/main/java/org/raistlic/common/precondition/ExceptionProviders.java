package org.raistlic.common.precondition;

import java.util.function.Function;

/**
 * @author Lei Chen (2015-10-14)
 */
abstract class ExceptionProviders {

  static Function<String, InvalidParameterException> invalidParameterExceptionProvider() {

    return InvalidParameterExceptionProvider.INSTANCE;
  }

  static Function<String, InvalidStateException> invalidStateExceptionProvider() {

    return InvalidStateExceptionProvider.INSTANCE;
  }

  private enum InvalidParameterExceptionProvider implements Function<String, InvalidParameterException> {

    INSTANCE;

    @Override
    public InvalidParameterException apply(String message) {

      return new InvalidParameterException(message);
    }
  }

  private enum InvalidStateExceptionProvider implements Function<String, InvalidStateException> {

    INSTANCE;

    @Override
    public InvalidStateException apply(String message) {

      return new InvalidStateException(message);
    }
  }

  private ExceptionProviders() { }
}
