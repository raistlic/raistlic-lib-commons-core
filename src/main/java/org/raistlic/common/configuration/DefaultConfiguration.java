package org.raistlic.common.configuration;

import org.raistlic.common.codec.Decoder;
import org.raistlic.common.codec.ValueConversionException;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * @author lei.c
 * @since 2014-12-28
 */
class DefaultConfiguration implements Configuration {

  private final Map<String, String> map;

  private final Set<String> keys;

  DefaultConfiguration(Map<String, String> map) {

    this.map = new HashMap<String, String>(map);
    this.keys = Collections.unmodifiableSet(map.keySet());
  }

  @Override
  public Set<String> getKeys() {

    return keys;
  }

  @Override
  public String getString(String key) {

    notNull(key);
    return map.get(key);
  }

  @Override
  public String getString(String key, String value) {

    String val = getString(key);
    return val == null ? value : val;
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

    notNull(decoder);

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

  private static void notNull(Object value) {

    if (value == null) {

      throw new NullPointerException();
    }
  }
}
