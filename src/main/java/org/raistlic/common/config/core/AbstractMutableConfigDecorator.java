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

import org.raistlic.common.codec.Decoder;
import org.raistlic.common.codec.Encoder;
import org.raistlic.common.config.source.ConfigSource;
import org.raistlic.common.precondition.Precondition;

import java.util.Collections;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author Lei Chen (2016-02-02)
 */
public abstract class AbstractMutableConfigDecorator implements MutableConfig {

  protected final MutableConfig mutableConfig;

  protected AbstractMutableConfigDecorator(MutableConfig mutableConfig) {

    Precondition.param(mutableConfig, "mutableConfig").isNotNull();
    this.mutableConfig = mutableConfig;
  }

  protected void configValueUpdated(String key) {

  }

  protected void configValuesUpdated(Iterable<String> keys) {

  }

  @Override
  public MutableConfig importFrom(ConfigSource configSource) {

    mutableConfig.importFrom(configSource);
    configValuesUpdated(configSource.getKeys());
    return this;
  }

  @Override
  public MutableConfig importFrom(Map<String, String> map) {

    mutableConfig.importFrom(map);
    configValuesUpdated(Collections.unmodifiableSet(map.keySet()));
    return this;
  }

  @Override
  public MutableConfig importFrom(Properties properties) {

    mutableConfig.importFrom(properties);
    configValuesUpdated(properties.keySet().stream().map(String::valueOf).collect(Collectors.toSet()));
    return this;
  }

  @Override
  public MutableConfig setString(String key, String value) {

    mutableConfig.setString(key, value);
    configValueUpdated(key);
    return this;
  }

  @Override
  public MutableConfig setBoolean(String key, boolean value) {

    mutableConfig.setBoolean(key, value);
    configValueUpdated(key);
    return this;
  }

  @Override
  public MutableConfig setByte(String key, byte value) {

    mutableConfig.setByte(key, value);
    configValueUpdated(key);
    return this;
  }

  @Override
  public MutableConfig setChar(String key, char value) {

    mutableConfig.setChar(key, value);
    configValueUpdated(key);
    return this;
  }

  @Override
  public MutableConfig setShort(String key, short value) {

    mutableConfig.setShort(key, value);
    configValueUpdated(key);
    return this;
  }

  @Override
  public MutableConfig setInt(String key, int value) {

    mutableConfig.setInt(key, value);
    configValueUpdated(key);
    return this;
  }

  @Override
  public MutableConfig setLong(String key, long value) {

    mutableConfig.setLong(key, value);
    configValueUpdated(key);
    return this;
  }

  @Override
  public MutableConfig setFloat(String key, float value) {

    mutableConfig.setFloat(key, value);
    configValueUpdated(key);
    return this;
  }

  @Override
  public MutableConfig setDouble(String key, double value) {

    mutableConfig.setDouble(key, value);
    configValueUpdated(key);
    return this;
  }

  @Override
  public <E> MutableConfig setValue(String key, E value, Encoder<? super E, String> encoder) {

    mutableConfig.setValue(key, value, encoder);
    configValueUpdated(key);
    return this;
  }

  @Override
  public Config get() {

    return mutableConfig.get();
  }

  @Override
  public String getString(String key, String value) {

    return mutableConfig.getString(key, value);
  }

  @Override
  public boolean getBoolean(String key, boolean value) {

    return mutableConfig.getBoolean(key, value);
  }

  @Override
  public byte getByte(String key, byte value) {

    return mutableConfig.getByte(key, value);
  }

  @Override
  public char getChar(String key, char value) {

    return mutableConfig.getChar(key, value);
  }

  @Override
  public short getShort(String key, short value) {

    return mutableConfig.getShort(key, value);
  }

  @Override
  public int getInt(String key, int value) {

    return mutableConfig.getInt(key, value);
  }

  @Override
  public long getLong(String key, long value) {

    return mutableConfig.getLong(key, value);
  }

  @Override
  public float getFloat(String key, float value) {

    return mutableConfig.getFloat(key, value);
  }

  @Override
  public double getDouble(String key, double value) {

    return mutableConfig.getDouble(key, value);
  }

  @Override
  public <E> E getValue(String key, Decoder<? extends E, String> decoder) {

    return mutableConfig.getValue(key, decoder);
  }

  @Override
  public <E> E getValue(String key, Decoder<? extends E, String> decoder, E value) {

    return mutableConfig.getValue(key, decoder, value);
  }

  @Override
  public void applyConfig(Config configuration) {

    mutableConfig.applyConfig(configuration);
    configValuesUpdated(configuration.getKeys());
  }

  @Override
  public Set<String> getKeys() {

    return mutableConfig.getKeys();
  }

  @Override
  public boolean hasKey(String key) {

    return mutableConfig.hasKey(key);
  }

  @Override
  public String getString(String key) {

    return mutableConfig.getString(key);
  }
}
