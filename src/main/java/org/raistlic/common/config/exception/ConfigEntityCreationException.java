package org.raistlic.common.config.exception;

/**
 * @author lei.c (2015-12-21)
 */
public class ConfigEntityCreationException extends ConfigException {

  public ConfigEntityCreationException(String message) {

    super(message);
  }

  public ConfigEntityCreationException(String message, Throwable cause) {

    super(message, cause);
  }

  public ConfigEntityCreationException(Throwable cause) {

    super(cause);
  }
}
