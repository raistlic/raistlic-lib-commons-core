package org.raistlic.common.precondition;

/**
 * @author lei.c
 * @since 2015-02-13
 */
public class NumberOutOfRangeException extends InvalidParameterException {

  public NumberOutOfRangeException(String message) {

    super(message);
  }

  public NumberOutOfRangeException(String message, Throwable cause) {

    super(message, cause);
  }

  public NumberOutOfRangeException(Throwable cause) {

    super(cause);
  }
}
