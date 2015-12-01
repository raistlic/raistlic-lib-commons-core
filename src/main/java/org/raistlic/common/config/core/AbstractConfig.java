package org.raistlic.common.config.core;

import org.raistlic.common.codec.Codec;
import org.raistlic.common.codec.Decoder;
import org.raistlic.common.codec.ValueConversionException;
import org.raistlic.common.config.converter.ConfigConverters;
import org.raistlic.common.config.exception.ConfigValueConvertException;

import static org.raistlic.common.precondition.Precondition.param;

/**
 * @author Lei Chen (2015-09-17)
 */
abstract class AbstractConfig implements Config {

  @Override
  public String getString(String key, String value) {

    param(key, "key").notNull();
    String val = getString(key);
    return val == null ? value : val;
  }

  @Override
  public boolean hasBoolean(String key) {

    param(key, "key").notNull();

    String val = getString(key);
    return val != null && BOOLEAN_CONVERTER.isValidDest(val);
  }

  @Override
  public boolean getBoolean(String key, boolean value) {

    param(key, "key").notNull();
    String val = getString(key);
    if (val == null) {
      return value;
    }
    return BOOLEAN_CONVERTER.decode(val);
  }

  @Override
  public boolean hasByte(String key) {

    param(key, "key").notNull();
    String val = getString(key);
    return val != null && BYTE_CONVERTER.isValidDest(val);
  }

  @Override
  public byte getByte(String key, byte value) {

    param(key, "key").notNull();
    String val = getString(key);
    return (val == null) ? value : BYTE_CONVERTER.decode(val);
  }

  @Override
  public boolean hasChar(String key) {

    param(key, "key").notNull();
    String val = getString(key);
    return val != null && CHAR_CONVERTER.isValidDest(val);
  }

  @Override
  public char getChar(String key, char value) {

    param(key, "key").notNull();
    String val = getString(key);
    return (val == null) ? value : CHAR_CONVERTER.decode(val);
  }

  @Override
  public boolean hasShort(String key) {

    param(key, "key").notNull();
    String val = getString(key);
    return val != null && SHORT_CONVERTER.isValidDest(val);
  }

  @Override
  public short getShort(String key, short value) {

    param(key, "key").notNull();
    String val = getString(key);
    if (val == null) {
      return value;
    }
    try {
      return Short.parseShort(val);
    }
    catch (NumberFormatException ex) {
      throw new ConfigValueConvertException(ex);
    }
  }

  @Override
  public boolean hasInt(String key) {

    param(key, "key").notNull();
    String val = getString(key);
    return val != null && INT_CONVERTER.isValidDest(val);
  }

  @Override
  public int getInt(String key, int value) {

    param(key, "key").notNull();
    String val = getString(key);
    return (val == null) ? value : INT_CONVERTER.decode(val);
  }

  @Override
  public boolean hasLong(String key) {

    param(key, "key").notNull();
    String val = getString(key);
    return val != null && LONG_CONVERTER.isValidDest(val);
  }

  @Override
  public long getLong(String key, long value) {

    param(key, "key").notNull();
    String val = getString(key);
    return (val == null) ? value : LONG_CONVERTER.decode(val);
  }

  @Override
  public boolean hasFloat(String key) {

    param(key, "key").notNull();
    String val = getString(key);
    return val != null && FLOAT_CONVERTER.isValidDest(val);
  }

  @Override
  public float getFloat(String key, float value) {

    param(key, "key").notNull();
    String val = getString(key);
    return (val == null) ? value : FLOAT_CONVERTER.decode(val);
  }

  @Override
  public boolean hasDouble(String key) {

    param(key, "key").notNull();
    String val = getString(key);
    return val != null && DOUBLE_CONVERTER.isValidDest(val);
  }

  @Override
  public double getDouble(String key, double value) {

    param(key, "key").notNull();
    String val = getString(key);
    return (val == null) ? value : DOUBLE_CONVERTER.decode(val);
  }

  @Override
  public boolean hasValue(String key, Decoder<?, String> decoder) {

    param(key, "key").notNull();
    param(decoder, "decoder").notNull();

    String val = getString(key);
    return val != null && decoder.isValidDest(val);
  }

  @Override
  public <E> E getValue(String key, Decoder<? extends E, String> decoder) {

    return getValue(key, decoder, null);
  }

  @Override
  public <E> E getValue(String key, Decoder<? extends E, String> decoder, E value) {

    param(key, "key").notNull();
    param(decoder, "decoder").notNull();

    String val = getString(key);
    if (val == null) {
      return value;
    }
    try {
      return decoder.decode(val);
    }
    catch (ValueConversionException ex) {
      throw new ConfigValueConvertException(ex);
    }
  }

  private static final Codec<Boolean, String> BOOLEAN_CONVERTER = ConfigConverters.booleanConverter();

  private static final Codec<Byte, String> BYTE_CONVERTER = ConfigConverters.byteConverter();

  private static final Codec<Character, String> CHAR_CONVERTER = ConfigConverters.charConverter();

  private static final Codec<Short, String> SHORT_CONVERTER = ConfigConverters.shortConverter();

  private static final Codec<Integer, String> INT_CONVERTER = ConfigConverters.intConverter();

  private static final Codec<Long, String> LONG_CONVERTER = ConfigConverters.longConverter();

  private static final Codec<Float, String> FLOAT_CONVERTER = ConfigConverters.floatConverter();

  private static final Codec<Double, String> DOUBLE_CONVERTER = ConfigConverters.doubleConverter();

}
