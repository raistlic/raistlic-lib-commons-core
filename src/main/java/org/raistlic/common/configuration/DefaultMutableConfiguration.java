package org.raistlic.common.configuration;

import org.raistlic.common.codec.Decoder;
import org.raistlic.common.codec.Encoder;
import org.raistlic.common.codec.ValueConversionException;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static org.raistlic.common.precondition.Precondition.param;

/**
 * @author Lei.C (2015-04-30)
 */
class DefaultMutableConfiguration implements MutableConfiguration {

  private final Map<String, String> map;

  DefaultMutableConfiguration() {

    map = new HashMap<String, String>();
  }

  @Override
  public void setString(String key, String value) {

    param(key, "key").notNull();
    map.put(key, value);
  }

  @Override
  public void setBoolean(String key, boolean value) {

    param(key, "key").notNull();
    map.put(key, String.valueOf(value));
  }

  @Override
  public void setByte(String key, byte value) {

    param(key, "key").notNull();
    map.put(key, String.valueOf(value));
  }

  @Override
  public void setChar(String key, char value) {

    param(key, "key").notNull();
    map.put(key, String.valueOf(value));
  }

  @Override
  public void setShort(String key, short value) {

    param(key, "key").notNull();
    map.put(key, String.valueOf(value));
  }

  @Override
  public void setInt(String key, int value) {

    param(key, "key").notNull();
    map.put(key, String.valueOf(value));
  }

  @Override
  public void setLong(String key, long value) {

    param(key, "key").notNull();
    map.put(key, String.valueOf(value));
  }

  @Override
  public void setFloat(String key, float value) {

    param(key, "key").notNull();
    map.put(key, String.valueOf(value));
  }

  @Override
  public void setDouble(String key, double value) {

    param(key, "key").notNull();
    map.put(key, String.valueOf(value));
  }

  @Override
  public <E> void setValue(String key, E value, Encoder<? super E, String> encoder) {

    param(key, "key").notNull();
    param(encoder, "encoder").notNull();

    try {
      String converted = encoder.encode(value);
      map.put(key, converted);
    }
    catch (ValueConversionException ex) {
      throw new ConfigurationValueConvertException(ex);
    }
  }

  @Override
  public Set<String> getKeys() {

    return Collections.unmodifiableSet(map.keySet());
  }

  @Override
  public String getString(String key) {

    param(key, "key").notNull();
    return map.get(key);
  }

  @Override
  public String getString(String key, String value) {

    String stored = getString(key);
    return stored == null ? value : stored;
  }

  @Override
  public boolean getBoolean(String key, boolean value) {

    String stored = getString(key);
    if (stored == null) {
      return value;
    }
    return Boolean.valueOf(stored);
  }

  @Override
  public byte getByte(String key, byte value) {

    String stored = getString(key);
    if (stored == null) {
      return value;
    }
    try {
      return Byte.parseByte(stored);
    }
    catch (NumberFormatException ex) {
      throw new ConfigurationValueConvertException(ex);
    }
  }

  @Override
  public char getChar(String key, char value) {

    String stored = getString(key);
    if (stored == null) {
      return value;
    }
    if (stored.length() != 1) {
      throw new ConfigurationValueConvertException("Invalid character value: '" + stored + "'");
    }
    return stored.charAt(0);
  }

  @Override
  public short getShort(String key, short value) {

    String stored = getString(key);
    if (stored == null) {
      return value;
    }
    try {
      return Short.parseShort(stored);
    }
    catch (NumberFormatException ex) {
      throw new ConfigurationValueConvertException(ex);
    }
  }

  @Override
  public int getInt(String key, int value) {

    String stored = getString(key);
    if (stored == null) {
      return value;
    }
    try {
      return Integer.parseInt(stored);
    }
    catch (NumberFormatException ex) {
      throw new ConfigurationValueConvertException(ex);
    }
  }

  @Override
  public long getLong(String key, long value) {

    String stored = getString(key);
    if (stored == null) {
      return value;
    }
    try {
      return Long.parseLong(stored);
    }
    catch (NumberFormatException ex) {
      throw new ConfigurationValueConvertException(ex);
    }
  }

  @Override
  public float getFloat(String key, float value) {

    String stored = getString(key);
    if (stored == null) {
      return value;
    }
    try {
      return Float.parseFloat(stored);
    }
    catch (NumberFormatException ex) {
      throw new ConfigurationValueConvertException(ex);
    }
  }

  @Override
  public double getDouble(String key, double value) {

    String stored = getString(key);
    if (stored == null) {
      return value;
    }
    try {
      return Double.parseDouble(stored);
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

    String stored = getString(key);
    if (stored == null) {
      return value;
    }
    try {
      return decoder.decode(stored);
    }
    catch (ValueConversionException ex) {
      throw new ConfigurationValueConvertException(ex);
    }
  }

  @Override
  public Configuration build() {

    return new DefaultConfiguration(map);
  }

  @Override
  public boolean isReady() {

    return true;
  }

  @Override
  public void applyConfig(Configuration configuration) {

    for (String key : configuration.getKeys()) {

      String val = configuration.getString(key);
      map.put(key, val);
    }
  }

  @Override
  public void extractConfig(Builder builder) {

    builder.applyConfig(this);
  }
}
