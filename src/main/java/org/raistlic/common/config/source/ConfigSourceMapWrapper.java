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

package org.raistlic.common.config.source;

import org.raistlic.common.precondition.Precondition;

import java.util.Collections;
import java.util.Map;
import java.util.Set;

/**
 * An instance of the class is immutable when the {@code map} passed into the constructor is
 * immutable.
 *
 * @author Lei Chen (2015-09-11)
 */
final class ConfigSourceMapWrapper implements ConfigSource {

  private final Map<String, String> map;

  private final Set<String> keys;

  ConfigSourceMapWrapper(Map<String, String> map) {

    this.map = map;
    this.keys = Collections.unmodifiableSet(map.keySet());
  }

  @Override
  public Set<String> getKeys() {

    return keys;
  }

  @Override
  public boolean hasKey(String key) {

    Precondition.param(key, "key").isNotNull();
    return keys.contains(key);
  }

  @Override
  public String getString(String key) {

    Precondition.param(key, "key").isNotNull();
    return map.get(key);
  }
}
