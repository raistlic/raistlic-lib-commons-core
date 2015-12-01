package org.raistlic.common.config.io;

import org.raistlic.common.config.core.Config;
import org.raistlic.common.config.core.ConfigBuilder;
import org.raistlic.common.config.exception.ConfigIOException;
import org.raistlic.common.config.source.ConfigSource;
import org.raistlic.common.precondition.InvalidParameterException;

import java.io.InputStream;
import java.io.OutputStream;

/**
 * The core interface for configuration input and output.
 *
 * @author Lei Chen (2015-09-14)
 */
public interface ConfigIO {

  /**
   * The method writes the specified {@code config} to the {@code outputStream} , the format of the
   * persisted configuration information is implementation specific, but it should be a format
   * that the same implementation would be able to read it back via the {@link #readConfig(ConfigBuilder, InputStream)}
   * method.
   *
   * @param config the config information to be persisted, cannot be {@code null}.
   * @param outputStream the output stream to write the {@code config} to, cannot be {@code null} and
   *                     must be open for write.
   *
   * @throws InvalidParameterException when {@code config} or {@code outputStream} is {@code null}.
   * @throws ConfigIOException when anything goes wrong during the process of writing the config.
   */
  void writeConfig(ConfigSource config, OutputStream outputStream)
      throws InvalidParameterException, ConfigIOException;

  /**
   * The method loads the configuration information from the specified {@code inputStream}, as
   * a {@link Config} instance. The format of the configuration information must be supported by
   * the {@link ConfigIO} implementation, in order to be loaded, see alse
   * {@link #writeConfig(ConfigSource, OutputStream)} .
   *
   * @param inputStream the input stream to load config information from, cannot be {@code null} and
   *                    must be open for read.
   * @return the loaded configuration information.
   *
   * @throws InvalidParameterException when {@code inputStream} is {@code null}.
   * @throws ConfigIOException when anything goes wrong during the process of loading the config.
   */
  Config readConfig(InputStream inputStream) throws ConfigIOException;

  /**
   * The method loads the configuration information from the specified {@code inputStream} into the
   * {@code configBuilder}. The format of the configuration information must be supported by
   * the {@link ConfigIO} implementation, in order to be loaded, see alse
   * {@link #writeConfig(ConfigSource, OutputStream)} .
   *
   * @param configBuilder the config builder to load the configuration information into, cannot be
   *                      {@code null}.
   * @param inputStream the input stream to load config information from, cannot be {@code null} and
   *                    must be open for read.
   *
   * @throws InvalidParameterException when {@code configBuilder} or {@code inputStream} is
   *         {@code null}.
   * @throws ConfigIOException when anything goes wrong during the process of loading the config.
   */
  void readConfig(ConfigBuilder configBuilder, InputStream inputStream) throws ConfigIOException;
}
