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

import org.raistlic.common.codec.Encoder;
import org.raistlic.common.config.source.ConfigSource;
import org.raistlic.common.util.Factory;

import java.util.Map;
import java.util.Properties;

/**
 * The interface defines a builder of {@link Config}, which provides setter methods to
 * specify the key-value pairs to be included in the created {@link Config} instance.
 *
 * @author Lei Chen (2015-09-10)
 */
public interface ConfigBuilder extends Configurable, Factory<Config> {

  /**
   * The method imports values from the {@code configSource}.
   *
   * @param configSource the source to import values from, cannot be {@code null}.
   * @return the {@link ConfigBuilder} instance itself.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code configSource}
   *         is {@code null}.
   */
  ConfigBuilder importFrom(ConfigSource configSource);

  /**
   * The method imports values from the {@code map} .
   *
   * @param map the map to import values from, cannot be {@code null}.
   * @return the {@link ConfigBuilder} instance itself.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code map} is {@code null}.
   */
  ConfigBuilder importFrom(Map<String, String> map);

  /**
   * The method imports values from the {@code properties} .
   *
   * @param properties the properties to import values from, cannot be {@code null}.
   * @return the {@link ConfigBuilder} instance itself.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code properties} is
   *         {@code null}.
   */
  ConfigBuilder importFrom(Properties properties);

  /**
   * The method sets a {@link String} {@code value} to be mapped under the {@code key} .
   *
   * @param key the key used to map the {@code value}, cannot be {@code null}.
   * @param value the value to be mapped.
   *
   * @return the {@link ConfigBuilder} instance itself.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} is {@code null}.
   */
  ConfigBuilder setString(String key, String value);

  /**
   * The method sets a {@code boolean} {@code value} to be mapped under the {@code key} .
   *
   * @param key the key used to map the {@code value}, cannot be {@code null}.
   * @param value the value to be mapped.
   *
   * @return the {@link ConfigBuilder} instance itself.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} is {@code null}.
   */
  ConfigBuilder setBoolean(String key, boolean value);

  /**
   * The method maps the {@code byte value} under the {@code key} .
   *
   * @param key the key used to map the {@code value}, cannot be {@code null}.
   * @param value the value to be mapped.
   *
   * @return the {@link ConfigBuilder} instance itself.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} is {@code null}.
   */
  ConfigBuilder setByte(String key, byte value);

  /**
   * The method maps the {@code char value} under the {@code key} .
   *
   * @param key the key used to map the {@code value}, cannot be {@code null}.
   * @param value the value to be mapped.
   *
   * @return the {@link ConfigBuilder} instance itself.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when if {@code key} is {@code null}.
   */
  ConfigBuilder setChar(String key, char value);

  /**
   * The method maps the {@code short value} under the {@code key} .
   *
   * @param key the key used to map the {@code value}, cannot be {@code null}.
   * @param value the value to be mapped.
   *
   * @return the {@link ConfigBuilder} instance itself.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} is {@code null}.
   */
  ConfigBuilder setShort(String key, short value);

  /**
   * The method maps the {@code int value} under the {@code key} .
   *
   * @param key the key used to map the {@code value}, cannot be {@code null}.
   * @param value the value to be mapped.
   *
   * @return the {@link ConfigBuilder} instance itself.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} is {@code null}.
   */
  ConfigBuilder setInt(String key, int value);

  /**
   * The method maps the {@code long value} under the {@code key} .
   *
   * @param key the key used to map the {@code value}, cannot be {@code null}.
   * @param value the value to be mapped.
   *
   * @return the {@link ConfigBuilder} instance itself.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} is {@code null}.
   */
  ConfigBuilder setLong(String key, long value);

  /**
   * The method maps the {@code float value} under the {@code key} .
   *
   * @param key the key used to map the {@code value}, cannot be {@code null}.
   * @param value the value to be mapped.
   *
   * @return the {@link ConfigBuilder} instance itself.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} is {@code null}.
   */
  ConfigBuilder setFloat(String key, float value);

  /**
   * The method maps the {@code double value} under the {@code key} .
   *
   * @param key the key used to map the {@code value}, cannot be {@code null}.
   * @param value the value to be mapped.
   *
   * @return the {@link ConfigBuilder} instance itself.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} is {@code null}.
   */
  ConfigBuilder setDouble(String key, double value);

  /**
   * The method maps the {@code value} under the {@code key} .
   *
   * @param key the key used to map the {@code value}, cannot be {@code null}.
   * @param value the value to be mapped.
   * @param encoder the encoder used to convert {@code value} into a {@link String} , cannot be
   *                {@code null}.
   * @param <E> the declared type for the {@code value} .
   *
   * @return the {@link ConfigBuilder} instance itself.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} or
   *         {@code encoder} is {@code null}.
   */
  <E> ConfigBuilder setValue(String key, E value, Encoder<? super E, String> encoder);
}
