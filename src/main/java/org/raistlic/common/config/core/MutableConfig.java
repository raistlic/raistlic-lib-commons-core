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

import java.util.Map;
import java.util.Properties;

/**
 * @author Lei Chen (2015-09-11)
 */
public interface MutableConfig extends Config, ConfigBuilder {

  MutableConfig importFrom(ConfigSource configSource);

  MutableConfig importFrom(Map<String, String> map);

  MutableConfig importFrom(Properties properties);

  MutableConfig setString(String key, String value);

  MutableConfig setBoolean(String key, boolean value);

  MutableConfig setByte(String key, byte value);

  MutableConfig setChar(String key, char value);

  MutableConfig setShort(String key, short value);

  MutableConfig setInt(String key, int value);

  MutableConfig setLong(String key, long value);

  MutableConfig setFloat(String key, float value);

  MutableConfig setDouble(String key, double value);

  <E> MutableConfig setValue(String key, E value, Encoder<? super E, String> encoder);
}
