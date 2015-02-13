package org.raistlic.common.precondition;

/**
 * @author lei.c
 * @since 2015-02-13
 */
public class InvalidStatusException extends PreconditionException {

  public InvalidStatusException(String message) {

    super(message);
  }

  public InvalidStatusException(String message, Throwable cause) {

    super(message, cause);
  }

  public InvalidStatusException(Throwable cause) {

    super(cause);
  }
}
