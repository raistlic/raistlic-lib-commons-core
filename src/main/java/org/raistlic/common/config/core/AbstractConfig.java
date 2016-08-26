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

import org.raistlic.common.codec.Codec;
import org.raistlic.common.codec.Decoder;
import org.raistlic.common.codec.ValueConversionException;
import org.raistlic.common.config.converter.ConfigConverters;
import org.raistlic.common.config.exception.ConfigValueConvertException;
import org.raistlic.common.precondition.Precondition;

import java.util.Optional;

/**
 * @author Lei Chen (2015-09-17)
 */
abstract class AbstractConfig implements Config {

  @Override
  public Optional<Boolean> getBoolean(String key) {

    return Optional.ofNullable(getString(key))
        .map(BOOLEAN_CONVERTER::decode);
  }

  @Override
  public Optional<Byte> getByte(String key) {

    return Optional.ofNullable(getString(key))
        .map(BYTE_CONVERTER::decode);
  }

  @Override
  public Optional<Character> getChar(String key) {

    return Optional.ofNullable(getString(key))
        .map(CHAR_CONVERTER::decode);
  }

  @Override
  public Optional<Short> getShort(String key) {

    return Optional.ofNullable(getString(key))
        .map(SHORT_CONVERTER::decode);
  }

  @Override
  public Optional<Integer> getInt(String key) {

    return Optional.ofNullable(getString(key))
        .map(INT_CONVERTER::decode);
  }

  @Override
  public Optional<Long> getLong(String key) {

    return Optional.ofNullable(getString(key))
        .map(LONG_CONVERTER::decode);
  }

  @Override
  public Optional<Float> getFloat(String key) {

    return Optional.ofNullable(getString(key))
        .map(FLOAT_CONVERTER::decode);
  }

  @Override
  public Optional<Double> getDouble(String key) {

    return Optional.ofNullable(getString(key))
        .map(DOUBLE_CONVERTER::decode);
  }

  @Override
  public <E> Optional<E> getValue(String key, Decoder<? extends E, String> decoder) {

    Precondition.param(decoder).isNotNull();

    try {
      return Optional.ofNullable(getString(key))
          .map(decoder::decode);
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
