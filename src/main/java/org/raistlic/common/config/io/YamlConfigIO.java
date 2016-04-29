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

import com.esotericsoftware.yamlbeans.YamlReader;
import com.esotericsoftware.yamlbeans.YamlWriter;
import org.raistlic.common.config.core.Config;
import org.raistlic.common.config.core.ConfigBuilder;
import org.raistlic.common.config.core.ConfigFactory;
import org.raistlic.common.config.exception.ConfigIOException;
import org.raistlic.common.config.source.ConfigSource;
import org.raistlic.common.precondition.InvalidParameterException;
import org.raistlic.common.precondition.Precondition;

import java.io.*;
import java.util.Map;

/**
 * @author Lei Chen (2015-10-12)
 */
enum YamlConfigIO implements ConfigIO {

  INSTANCE;

  @Override
  public void writeConfig(ConfigSource config, OutputStream outputStream)
      throws InvalidParameterException, ConfigIOException {

    Precondition.param(config, "config").isNotNull();
    Precondition.param(outputStream, "outputStream").isNotNull();

    try {
      Map<String, Object> map = NestedMapHelper.configToMap(config);
      ByteArrayOutputStream buffer = new ByteArrayOutputStream();
      OutputStreamWriter writer = new OutputStreamWriter(buffer);
      YamlWriter yamlWriter = new YamlWriter(writer);
      yamlWriter.write(map);
      yamlWriter.close();
      // use PrintStream because YamlWriter is handling new line characters
      // in a wrong way (not cross OS proof), which causes tests to fail in
      // Windows.
      PrintStream ps = new PrintStream(outputStream);
      for (String line : new String(buffer.toByteArray()).split("\n")) {
        ps.println(line);
      }
    }
    catch (Exception ex) {
      throw new ConfigIOException(ex);
    }
  }

  @Override
  public Config readConfig(InputStream inputStream) throws ConfigIOException {

    Precondition.param(inputStream, "inputStream").isNotNull();

    ConfigBuilder configBuilder = ConfigFactory.newMutableConfig();
    readConfig(configBuilder, inputStream);
    return configBuilder.get();
  }

  @Override
  public void readConfig(ConfigBuilder configBuilder, InputStream inputStream) throws ConfigIOException {

    Precondition.param(configBuilder, "configBuilder").isNotNull();
    Precondition.param(inputStream, "inputStream").isNotNull();

    try {
      InputStreamReader reader = new InputStreamReader(inputStream);
      YamlReader yamlReader = new YamlReader(reader);
      @SuppressWarnings("unchecked")
      Map<String, Object> map = (Map<String, Object>) yamlReader.read();
      NestedMapHelper.mapToConfig(map, "", configBuilder);
    }
    catch (Exception ex) {
      throw new ConfigIOException(ex);
    }
  }
}
