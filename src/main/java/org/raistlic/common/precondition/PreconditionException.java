package org.raistlic.common.precondition;

/**
 * @author lei.c
 * @since 2015-02-13
 */
public class PreconditionException extends RuntimeException {

  public PreconditionException(String message) {

    super(message);
  }

  public PreconditionException(String message, Throwable cause) {

    super(message, cause);
  }

  public PreconditionException(Throwable cause) {

    super(cause);
  }
}
