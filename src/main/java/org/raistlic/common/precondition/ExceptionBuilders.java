package org.raistlic.common.precondition;

import org.raistlic.common.Factory;
import org.raistlic.common.WeakThreadLocal;

/**
 * @author Lei Chen (2015-10-14)
 */
abstract class ExceptionBuilders {

  static ExceptionBuilder<InvalidParameterException> invalidParameterExceptionExceptionBuilder() {

    return InvalidParameterExceptionBuilder.LOCAL_INSTANCE.get();
  }

  private static class InvalidParameterExceptionBuilder
      implements ExceptionBuilder<InvalidParameterException> {

    private static final WeakThreadLocal<InvalidParameterExceptionBuilder> LOCAL_INSTANCE =
        new WeakThreadLocal<InvalidParameterExceptionBuilder>(new Factory<InvalidParameterExceptionBuilder>() {

          @Override
          public InvalidParameterExceptionBuilder build() {

            return new InvalidParameterExceptionBuilder();
          }

          @Override
          public boolean isReady() {

            return true;
          }
        });

    private Throwable cause;

    private String message;

    @Override
    public ExceptionBuilder<InvalidParameterException> withMessage(String message) {

      this.message = message;
      return this;
    }

    @Override
    public ExceptionBuilder<InvalidParameterException> withCause(Throwable cause) {

      this.cause = cause;
      return this;
    }

    @Override
    public InvalidParameterException build() {

      return new InvalidParameterException(message, cause);
    }

    @Override
    public boolean isReady() {

      return true;
    }
  }

  private ExceptionBuilders() { }
}
