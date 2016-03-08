/*
 * Copyright 2015 Lei CHEN (raistlic@gmail.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
    return configBuilder.get();
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
