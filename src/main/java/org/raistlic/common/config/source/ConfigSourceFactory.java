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
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

/**
 * Static factory methods holder, exports different types of {@link ConfigSource} instances.
 *
 * @author Lei Chen (2015-09-11)
 */
public final class ConfigSourceFactory {

  /*
   * The immutable empty singleton.
   */
  private static final ConfigSource IMMUTABLE_EMPTY_INSTANCE = wrap(Collections.<String, String>emptyMap());

  /**
   * The method returns a singleton instance of an immutable, empty {@link ConfigSource} .
   *
   * @return a singleton instance of an immutable, empty {@link ConfigSource} .
   */
  public static ConfigSource immutableEmptySource() {

    return IMMUTABLE_EMPTY_INSTANCE;
  }

  /**
   * The method creates an immutable {@link ConfigSource} based on the values from the specified
   * {@code map} .
   *
   * @param map the map based on which values to create the instance, cannot be {@code null}.
   * @return the immutable instance created.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code map} is {@code null}.
   */
  public static ConfigSource immutableCopyOf(Map<String, String> map) {

    Precondition.param(map).isNotNull();
    Map<String, String> copy = map.isEmpty() ?
        Collections.<String, String>emptyMap() :
        Collections.unmodifiableMap(new HashMap<String, String>(map));
    return wrap(copy);
  }

  /**
   * The method creates an immutable {@link ConfigSource} based on the values from the specified
   * {@code properties} .
   *
   * @param properties the properties based on which values to create the instance, cannot be {@code null}.
   * @return the immutable instance created.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code properties} is
   *         {@code null}.
   */
  public static ConfigSource immutableCopyOf(Properties properties) {

    Precondition.param(properties).isNotNull();
    Set<Object> keySet = properties.keySet();
    if (keySet.isEmpty()) {
      return IMMUTABLE_EMPTY_INSTANCE;
    }
    Map<String, String> copy = new HashMap<String, String>();
    for (Object keyObject : keySet) {
      String key = String.valueOf(keyObject);
      String value = properties.getProperty(key);
      copy.put(key, value);
    }
    return wrap(copy);
  }

  /**
   * The method creates an immutable {@link ConfigSource} copy based on the values from the given
   * {@code configSource} .
   *
   * @param configSource the config source instance based on which values to create the copy, cannot
   *                     be {@code null}.
   * @return the immutable instance created.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code properties} is
   *         {@code null}.
   */
  public static ConfigSource immutableCopyOf(ConfigSource configSource) {

    Precondition.param(configSource).isNotNull();
    Set<String> keySet = configSource.getKeys();
    if (keySet.isEmpty()) {
      return IMMUTABLE_EMPTY_INSTANCE;
    }
    Map<String, String> copy = new HashMap<String, String>();
    for (String key : keySet) {
      copy.put(key, configSource.getString(key));
    }
    return wrap(copy);
  }

  /**
   * The method wraps the specified {@code map} and creates an instance that adapts to the
   * {@link ConfigSource} interface, which redirects all queries to the {@code map} .
   *
   * @param map the map to be wrapped, cannot be {@code null}.
   * @return the created wrapper instance.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code map} is {@code null}.
   */
  public static ConfigSource wrap(Map<String, String> map) {

    Precondition.param(map).isNotNull();
    return new ConfigSourceMapWrapper(map);
  }

  /**
   * The method wraps the specified {@code properties} and creates an instance that adapts to the
   * {@link ConfigSource} interface, which redirects all queries to the {@code properties} .
   *
   * @param properties the properties to be wrapped, cannot be {@code null}.
   * @return the created wrapper instance.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code map} is {@code null}.
   */
  public static ConfigSource wrap(Properties properties) {

    Precondition.param(properties).isNotNull();
    return new ConfigSourcePropertiesWrapper(properties);
  }

  /*
   * Not to be instantiated or inherited.
   */
  private ConfigSourceFactory() { }
}
