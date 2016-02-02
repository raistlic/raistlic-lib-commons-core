package org.raistlic.common.config.core;

import org.raistlic.common.codec.Decoder;
import org.raistlic.common.codec.Encoder;
import org.raistlic.common.config.source.ConfigSource;
import org.raistlic.common.precondition.Precondition;

import java.util.Map;
import java.util.Properties;
import java.util.Set;

/**
 * @author Lei Chen (2016-02-02)
 */
public abstract class AbstractMutableConfigDecorator implements MutableConfig {

  protected final MutableConfig mutableConfig;

  protected AbstractMutableConfigDecorator(MutableConfig mutableConfig) {

    Precondition.param(mutableConfig, "mutableConfig").notNull();
    this.mutableConfig = mutableConfig;
  }

  @Override
  public MutableConfig importFrom(ConfigSource configSource) {

    return mutableConfig.importFrom(configSource);
  }

  @Override
  public MutableConfig importFrom(Map<String, String> map) {

    return mutableConfig.importFrom(map);
  }

  @Override
  public MutableConfig importFrom(Properties properties) {

    return mutableConfig.importFrom(properties);
  }

  @Override
  public MutableConfig setString(String key, String value) {

    return mutableConfig.setString(key, value);
  }

  @Override
  public MutableConfig setBoolean(String key, boolean value) {

    return mutableConfig.setBoolean(key, value);
  }

  @Override
  public MutableConfig setByte(String key, byte value) {

    return mutableConfig.setByte(key, value);
  }

  @Override
  public MutableConfig setChar(String key, char value) {

    return mutableConfig.setChar(key, value);
  }

  @Override
  public MutableConfig setShort(String key, short value) {

    return mutableConfig.setShort(key, value);
  }

  @Override
  public MutableConfig setInt(String key, int value) {

    return mutableConfig.setInt(key, value);
  }

  @Override
  public MutableConfig setLong(String key, long value) {

    return mutableConfig.setLong(key, value);
  }

  @Override
  public MutableConfig setFloat(String key, float value) {

    return mutableConfig.setFloat(key, value);
  }

  @Override
  public MutableConfig setDouble(String key, double value) {

    return mutableConfig.setDouble(key, value);
  }

  @Override
  public <E> MutableConfig setValue(String key, E value, Encoder<? super E, String> encoder) {

    return mutableConfig.setValue(key, value, encoder);
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
