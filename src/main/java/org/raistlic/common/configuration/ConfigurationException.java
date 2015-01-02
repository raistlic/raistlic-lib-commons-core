package org.raistlic.common.configuration;

/**
 * @author lei.c
 * @since 2014-12-28
 */
public class ConfigurationException extends RuntimeException {

  protected ConfigurationException(String message) {

    super(message);
  }

  protected ConfigurationException(Throwable cause) {

    super(cause);
  }

  protected ConfigurationException(String message, Throwable cause) {

    super(message, cause);
  }
}
