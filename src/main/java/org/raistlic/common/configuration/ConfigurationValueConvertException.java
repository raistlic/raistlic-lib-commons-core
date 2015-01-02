package org.raistlic.common.configuration;

/**
 * @author lei.c
 * @since 2014-12-28
 */
public class ConfigurationValueConvertException extends ConfigurationException {

  protected ConfigurationValueConvertException(String message) {

    super(message);
  }

  protected ConfigurationValueConvertException(Throwable cause) {

    super(cause);
  }

  protected ConfigurationValueConvertException(String message, Throwable cause) {

    super(message, cause);
  }
}
