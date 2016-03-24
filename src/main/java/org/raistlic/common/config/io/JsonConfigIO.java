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

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import org.raistlic.common.config.core.Config;
import org.raistlic.common.config.core.ConfigBuilder;
import org.raistlic.common.config.core.ConfigFactory;
import org.raistlic.common.config.exception.ConfigIOException;
import org.raistlic.common.config.source.ConfigSource;
import org.raistlic.common.precondition.InvalidParameterException;
import org.raistlic.common.precondition.Precondition;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Lei Chen (2015-10-12)
 */
enum JsonConfigIO implements ConfigIO {

  INSTANCE;

  private final ObjectMapper objectMapper;

  JsonConfigIO() {

    objectMapper = new ObjectMapper();
    objectMapper.configure(SerializationFeature.INDENT_OUTPUT, true);
  }

  @Override
  public void writeConfig(ConfigSource config, OutputStream outputStream)
      throws InvalidParameterException, ConfigIOException {

    Precondition.param(config, "config").isNotNull();
    Precondition.param(outputStream, "outputStream").isNotNull();

    Map<String, Object> map = NestedMapHelper.configToMap(config);
    try {
      objectMapper.writer().writeValue(outputStream, map);
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
      Map<String, Object> jsonMap = objectMapper.readValue(inputStream, new TypeReference<HashMap<String, Object>>() {

      });
      NestedMapHelper.mapToConfig(jsonMap, "", configBuilder);
    }
    catch (Exception ex) {
      throw new ConfigIOException(ex);
    }
  }

}
