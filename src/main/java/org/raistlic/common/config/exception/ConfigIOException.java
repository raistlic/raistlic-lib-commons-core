package org.raistlic.common.config.exception;

/**
 * @author Lei Chen (2015-10-11)
 */
public class ConfigIOException extends ConfigException {

  public ConfigIOException(String message) {

    super(message);
  }

  public ConfigIOException(String message, Throwable cause) {

    super(message, cause);
  }

  public ConfigIOException(Throwable cause) {

    super(cause);
  }
}
