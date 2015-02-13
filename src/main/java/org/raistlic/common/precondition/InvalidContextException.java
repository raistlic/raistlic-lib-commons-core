package org.raistlic.common.precondition;

/**
 * @author lei.c
 * @since 2015-02-13
 */
public class InvalidContextException extends PreconditionException {

  public InvalidContextException(String message) {

    super(message);
  }

  public InvalidContextException(String message, Throwable cause) {

    super(message, cause);
  }

  public InvalidContextException(Throwable cause) {

    super(cause);
  }
}
