package org.raistlic.common.precondition;

/**
 * @author lei.c
 * @since 2015-02-13
 */
public class InvalidThreadException extends InvalidContextException {

  public InvalidThreadException(String message) {

    super(message);
  }

  public InvalidThreadException(String message, Throwable cause) {

    super(message, cause);
  }

  public InvalidThreadException(Throwable cause) {

    super(cause);
  }
}
