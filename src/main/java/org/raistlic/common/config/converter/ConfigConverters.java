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

package org.raistlic.common.config.converter;

import org.raistlic.common.codec.Codec;
import org.raistlic.common.config.core.Config;
import org.raistlic.common.config.exception.ConfigValueConvertException;

/**
 * The static factory methods that exports converters between different types of values and {@link String} ,
 * to be used in {@link Config} related implementations.
 *
 * @author Lei Chen (2015-09-14)
 */
public final class ConfigConverters {

  /**
   * The method returns a singleton, stateless {@code Codec<Boolean, String>} instance, which will
   * allow {@code null} values to pass through, and will throw {@link ConfigValueConvertException}
   * in case of a value conversion failure.
   *
   * @return the singleton {@link Codec} instance.
   */
  public static Codec<Boolean, String> booleanConverter() {

    return DefaultBooleanConverter.INSTANCE;
  }

  /**
   * The method returns a singleton, stateless {@code Codec<Character, String>} instance, which will
   * allow {@code null} values to pass through, and will throw {@link ConfigValueConvertException}
   * in case of a value conversion failure.
   *
   * @return the singleton {@link Codec} instance.
   */
  public static Codec<Byte, String> byteConverter() {

    return DefaultByteConverter.INSTANCE;
  }

  /**
   * The method returns a singleton, stateless {@code Codec<Character, String>} instance, which will
   * allow {@code null} values to pass through, and will throw {@link ConfigValueConvertException}
   * in case of a value conversion failure.
   *
   * @return the singleton {@link Codec} instance.
   */
  public static Codec<Character, String> charConverter() {

    return DefaultCharConverter.INSTANCE;
  }

  /**
   * The method returns a singleton, stateless {@code Codec<Short, String>} instance, which will
   * allow {@code null} values to pass through, and will throw {@link ConfigValueConvertException}
   * in case of a value conversion failure.
   *
   * @return the singleton {@link Codec} instance.
   */
  public static Codec<Short, String> shortConverter() {

    return DefaultShortConverter.INSTANCE;
  }

  /**
   * The method returns a singleton, stateless {@code Codec<Integer, String>} instance, which will
   * allow {@code null} values to pass through, and will throw {@link ConfigValueConvertException}
   * in case of a value conversion failure.
   *
   * @return the singleton {@link Codec} instance.
   */
  public static Codec<Integer, String> intConverter() {

    return DefaultIntConverter.INSTANCE;
  }

  /**
   * The method returns a singleton, stateless {@code Codec<Long, String>} instance, which will
   * allow {@code null} values to pass through, and will throw {@link ConfigValueConvertException}
   * in case of a value conversion failure.
   *
   * @return the singleton {@link Codec} instance.
   */
  public static Codec<Long, String> longConverter() {

    return DefaultLongConverter.INSTANCE;
  }

  /**
   * The method returns a singleton, stateless {@code Codec<Float, String>} instance, which will
   * allow {@code null} values to pass through, and will throw {@link ConfigValueConvertException}
   * in case of a value conversion failure.
   *
   * @return the singleton {@link Codec} instance.
   */
  public static Codec<Float, String> floatConverter() {

    return DefaultFloatConverter.INSTANCE;
  }

  /**
   * The method returns a singleton, stateless {@code Codec<Double, String>} instance, which will
   * allow {@code null} values to pass through, and will throw {@link ConfigValueConvertException}
   * in case of a value conversion failure.
   *
   * @return the singleton {@link Codec} instance.
   */
  public static Codec<Double, String> doubleConverter() {

    return DefaultDoubleConverter.INSTANCE;
  }

  /*
   * Not to be instantiated or inherited.
   */
  private ConfigConverters() {
  }
}
