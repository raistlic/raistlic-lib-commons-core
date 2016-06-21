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

package org.raistlic.common.config.core;

import org.raistlic.common.config.source.ConfigSource;

import java.util.Set;

import static org.raistlic.common.precondition.Precondition.param;

/**
 * An instance of the class is immutable when {@code configSource} passed into the constructor is
 * immutable.
 *
 * @author Lei Chen (2015-09-11)
 */
final class ConfigSourceWrapper extends AbstractConfig implements Config {

  private final ConfigSource configSource;

  ConfigSourceWrapper(ConfigSource configSource) {

    param(configSource).isNotNull();
    this.configSource = configSource;
  }

  @Override
  public Set<String> getKeys() {

    return configSource.getKeys();
  }

  @Override
  public boolean hasKey(String key) {

    return configSource.hasKey(key);
  }

  @Override
  public String getString(String key) {

    return configSource.getString(key);
  }
}
