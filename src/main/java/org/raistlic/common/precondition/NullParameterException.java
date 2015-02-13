package org.raistlic.common.precondition;

/**
 * @author lei.c
 * @since 2015-02-13
 */
public class NullParameterException extends InvalidParameterException {

  public NullParameterException(String paramName) {

    super("'" + paramName + "' cannot be null.");
  }

  public NullParameterException() {

    super("null parameter.");
  }
}
