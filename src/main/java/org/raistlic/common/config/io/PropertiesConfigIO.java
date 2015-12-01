package org.raistlic.common.config.io;

import org.raistlic.common.config.core.Config;
import org.raistlic.common.config.core.ConfigBuilder;
import org.raistlic.common.config.core.ConfigFactory;
import org.raistlic.common.config.exception.ConfigIOException;
import org.raistlic.common.config.source.ConfigSource;
import org.raistlic.common.precondition.Precondition;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Properties;

/**
 * @author Lei Chen (2015-10-11)
 */
enum PropertiesConfigIO implements ConfigIO {

  INSTANCE;

  @Override
  public void writeConfig(ConfigSource config, OutputStream outputStream) throws ConfigIOException {

    Precondition.param(config, "config").notNull();
    Precondition.param(outputStream, "outputStream").notNull();

    Properties properties = new Properties();
    for (String key : config.getKeys()) {
      properties.setProperty(key, config.getString(key));
    }
    try {
      ByteArrayOutputStream buffer = new ByteArrayOutputStream();
      properties.store(buffer, "");
      PrintStream ps = new PrintStream(outputStream);
      String[] lines = new String(buffer.toByteArray()).split("\n");
      for (String line : lines) {
        line = line.trim();
        if (!line.isEmpty() && !line.startsWith("#")) {
          ps.println(line);
        }
      }
    }
    catch (Exception ex) {
      throw new ConfigIOException(ex);
    }
  }

  @Override
  public Config readConfig(InputStream inputStream) throws ConfigIOException {

    Precondition.param(inputStream, "inputStream").notNull();

    ConfigBuilder configBuilder = ConfigFactory.newMutableConfig();
    readConfig(configBuilder, inputStream);
    return configBuilder.build();
  }

  @Override
  public void readConfig(ConfigBuilder configBuilder, InputStream inputStream) throws ConfigIOException {

    Precondition.param(configBuilder, "configBuilder").notNull();
    Precondition.param(inputStream, "inputStream").notNull();

    Properties properties = new Properties();
    try {
      properties.load(inputStream);
      configBuilder.importFrom(properties);
    }
    catch (Exception ex) {
      throw new ConfigIOException(ex);
    }
  }
}
