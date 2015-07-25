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

package org.raistlic.common.configuration;

import org.raistlic.common.codec.Decoder;
import org.raistlic.common.codec.ValueConversionException;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static org.raistlic.common.precondition.Precondition.param;

/**
 * @author Lei CHEN (2014-12-28)
 * @since 1.0
 */
final class DefaultConfiguration implements Configuration {

  private final Map<String, String> map;

  DefaultConfiguration(Map<String, String> map) {

    this.map = Collections.unmodifiableMap(new HashMap<String, String>(map));
  }

  @Override
  public Set<String> getKeys() {

    return map.keySet();
  }

  @Override
  public String getString(String key) {

    param(key, "key").notNull();
    return map.get(key);
  }

  @Override
  public String getString(String key, String value) {

    String val = getString(key);
    return val == null ? value : val;
  }

  @Override
  public boolean getBoolean(String key, boolean value) {

    String val = getString(key);
    return val == null ? value : Boolean.valueOf(val);
  }

  @Override
  public byte getByte(String key, byte value) {

    String val = getString(key);
    if (val == null) {
      return value;
    }
    try {
      return Byte.parseByte(val);
    }
    catch (NumberFormatException ex) {
      throw new ConfigurationValueConvertException(ex);
    }
  }

  @Override
  public char getChar(String key, char value) {

    String val = getString(key);
    if (val == null || val.isEmpty()) {
      return value;
    }
    else {
      return val.charAt(0);
    }
  }

  @Override
  public short getShort(String key, short value) {

    String val = getString(key);
    if (val == null) {
      return value;
    }
    try {
      return Short.parseShort(val);
    }
    catch (NumberFormatException ex) {
      throw new ConfigurationValueConvertException(ex);
    }
  }

  @Override
  public int getInt(String key, int value) {

    String val = getString(key);
    if (val == null) {
      return value;
    }
    try {
      return Integer.parseInt(val);
    }
    catch (NumberFormatException ex) {
      throw new ConfigurationValueConvertException(ex);
    }
  }

  @Override
  public long getLong(String key, long value) {

    String val = getString(key);
    if (val == null) {
      return value;
    }
    try {
      return Long.parseLong(val);
    }
    catch (NumberFormatException ex) {
      throw new ConfigurationValueConvertException(ex);
    }
  }

  @Override
  public float getFloat(String key, float value) {

    String val = getString(key);
    if (val == null) {
      return value;
    }
    try {
      return Float.parseFloat(val);
    }
    catch (NumberFormatException ex) {
      throw new ConfigurationValueConvertException(ex);
    }
  }

  @Override
  public double getDouble(String key, double value) {

    String val = getString(key);
    if (val == null) {
      return value;
    }
    try {
      return Double.parseDouble(val);
    }
    catch (NumberFormatException ex) {
      throw new ConfigurationValueConvertException(ex);
    }
  }

  @Override
  public <E> E getValue(String key, Decoder<? extends E, String> decoder) {

    return getValue(key, decoder, null);
  }

  @Override
  public <E> E getValue(String key, Decoder<? extends E, String> decoder, E value) {

    String val = getString(key);
    if (val == null) {
      return value;
    }
    try {
      return decoder.decode(val);
    }
    catch (ValueConversionException ex) {
      throw new ConfigurationValueConvertException(ex);
    }
  }
}
