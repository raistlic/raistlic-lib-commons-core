package org.raistlic.common.precondition;

/**
 * @author lei.c
 * @since 2015-02-13
 */
public class InvalidParameterException extends PreconditionException {

  public InvalidParameterException(String message) {

    super(message);
  }

  public InvalidParameterException(String message, Throwable cause) {

    super(message, cause);
  }

  public InvalidParameterException(Throwable cause) {

    super(cause);
  }
}
