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
import org.raistlic.common.config.source.ConfigSource;

import java.util.Optional;

/**
 * The interface abstracts the functionality of a configuration entity instance, which include
 * queries to multiple types of values using {@link String} key.
 *
 * @author Lei CHEN (2014-12-28)
 * @since 1.0
 */
public interface Config extends ConfigSource {

  /**
   * The method queries the {@link String} value specified by the {@code key} .
   *
   * @param key   the key to search, cannot be {@code null}.
   * @param value the default value to return in case {@code key} is not found.
   * @return the value in the {@link Config} specified by {@code key} , or the default
   * {@code value} parameter in case {@code key} is not found.
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} is {@code null}.
   */
  default String getString(String key, String value) {

    return Optional.ofNullable(getString(key))
      .orElse(value);
  }

  /**
   * The method queries the {@code boolean} value specified by the {@code key} .
   *
   * @param key   the key to search, cannot be {@code null}.
   * @param value the default value to return in case {@code key} is not found.
   * @return the value mapped for {@code key} in the {@link Config}, or the default
   * {@code value} parameter in case {@code key} is not found.
   * @throws org.raistlic.common.precondition.InvalidParameterException       when {@code key} is {@code null}.
   * @throws org.raistlic.common.config.exception.ConfigValueConvertException when there is a value
   *                                                                          found for the {@code key} , but error occur when trying to convert it to a {@code boolean}
   *                                                                          value. See also {@link #hasKey(String)} .
   */
  default boolean getBoolean(String key, boolean value) {

    return getBoolean(key).orElse(value);
  }

  /**
   * The method queries the {@code Boolean} value specified by the {@code key} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @return the value mapped for {@code key} in the {@link Config}, or an empty {@link Optional} if it's not found.
   * @throws org.raistlic.common.precondition.InvalidParameterException       when {@code key} is {@code null}.
   * @throws org.raistlic.common.config.exception.ConfigValueConvertException when there is a value
   *                                                                          found for the {@code key} , but error occur when trying to convert it to a {@code boolean}
   *                                                                          value. See also {@link #hasKey(String)} .
   */
  Optional<Boolean> getBoolean(String key);

  /**
   * The method queries the {@code byte} value specified by the {@code key} .
   *
   * @param key   the key to search, cannot be {@code null}.
   * @param value the default value to be returned, in case {@code key} is not found.
   * @return the {@code byte} value found in the {@link Config} by the {@code key} , or
   * the default {@code value} parameter in case {@code key} is not found.
   * @throws org.raistlic.common.precondition.InvalidParameterException       when {@code key} is {@code null}.
   * @throws org.raistlic.common.config.exception.ConfigValueConvertException if a value is found
   *                                                                          for the {@code key} , but failed when being converted to {@code byte} .
   */
  default byte getByte(String key, byte value) {

    return getByte(key).orElse(value);
  }

  /**
   * The method queries the {@code byte} value specified by the {@code key} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @return the {@code byte} value found in the {@link Config}, or an empty {@link Optional} if it's not found.
   * @throws org.raistlic.common.precondition.InvalidParameterException       when {@code key} is {@code null}.
   * @throws org.raistlic.common.config.exception.ConfigValueConvertException if a value is found
   *                                                                          for the {@code key} , but failed when being converted to {@code byte} .
   */
  Optional<Byte> getByte(String key);

  /**
   * The method queries the {@code char} value specified by the {@code key} .
   *
   * @param key   the key to search, cannot be {@code null}.
   * @param value the default value to be returned, in case {@code key} is not found.
   * @return the {@code char} value found in the {@link Config} by the {@code key} , or
   * the default {@code value} parameter in case {@code key} is not found.
   * @throws org.raistlic.common.precondition.InvalidParameterException       when {@code key} is {@code null}.
   * @throws org.raistlic.common.config.exception.ConfigValueConvertException if a value is found
   *                                                                          for the {@code key} , but failed when being converted to {@code char} .
   */
  default char getChar(String key, char value) {

    return getChar(key).orElse(value);
  }

  /**
   * The method queries the {@code char} value specified by the {@code key} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @return the {@code char} value found in the {@link Config}, or an empty {@link Optional} if it's not found.
   * @throws org.raistlic.common.precondition.InvalidParameterException       when {@code key} is {@code null}.
   * @throws org.raistlic.common.config.exception.ConfigValueConvertException if a value is found
   *                                                                          for the {@code key} , but failed when being converted to {@code char} .
   */
  Optional<Character> getChar(String key);

  /**
   * The method queries the {@code short} value specified by the {@code key} .
   *
   * @param key   the key to search, cannot be {@code null}.
   * @param value the default value to be returned, in case {@code key} is not found.
   * @return the {@code short} value found in the {@link Config} by the {@code key} , or
   * the default {@code value} parameter in case {@code key} is not found.
   * @throws org.raistlic.common.precondition.InvalidParameterException       when {@code key} is {@code null}.
   * @throws org.raistlic.common.config.exception.ConfigValueConvertException if a value is found
   *                                                                          for the {@code key} , but failed when being converted to {@code short} .
   */
  default short getShort(String key, short value) {

    return getShort(key).orElse(value);
  }

  /**
   * The method queries the {@code short} value specified by the {@code key} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @return the {@code short} value found in the {@link Config}, or an empty {@link Optional} if it's not found.
   * @throws org.raistlic.common.precondition.InvalidParameterException       when {@code key} is {@code null}.
   * @throws org.raistlic.common.config.exception.ConfigValueConvertException if a value is found
   *                                                                          for the {@code key} , but failed when being converted to {@code short} .
   */
  Optional<Short> getShort(String key);

  /**
   * The method queries the {@code int} value specified by the {@code key} .
   *
   * @param key   the key to search, cannot be {@code null}.
   * @param value the default value to be returned, in case {@code key} is not found.
   * @return the {@code int} value found in the {@link Config} by the {@code key} , or
   * the default {@code value} parameter in case {@code key} is not found.
   * @throws org.raistlic.common.precondition.InvalidParameterException       when {@code key} is {@code null}.
   * @throws org.raistlic.common.config.exception.ConfigValueConvertException if a value is found
   *                                                                          for the {@code key} , but failed when being converted to {@code int} .
   */
  default int getInt(String key, int value) {

    return getInt(key).orElse(value);
  }

  /**
   * The method queries the {@code int} value specified by the {@code key} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @return the {@code int} value found in the {@link Config}, or an empty {@link Optional} if it's not found.
   * @throws org.raistlic.common.precondition.InvalidParameterException       when {@code key} is {@code null}.
   * @throws org.raistlic.common.config.exception.ConfigValueConvertException if a value is found
   *                                                                          for the {@code key} , but failed when being converted to {@code int} .
   */
  Optional<Integer> getInt(String key);

  /**
   * The method queries the {@code long} value specified by the {@code key} .
   *
   * @param key   the key to search, cannot be {@code null}.
   * @param value the default value to be returned, in case {@code key} is not found.
   * @return the {@code long} value found in the {@link Config} by the {@code key} , or
   * the default {@code value} parameter in case {@code key} is not found.
   * @throws org.raistlic.common.precondition.InvalidParameterException       when {@code key} is {@code null}.
   * @throws org.raistlic.common.config.exception.ConfigValueConvertException if a value is found
   *                                                                          for the {@code key} , but failed when being converted to {@code long} .
   */
  default long getLong(String key, long value) {

    return getLong(key).orElse(value);
  }

  /**
   * The method queries the {@code long} value specified by the {@code key} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @return the {@code long} value found in the {@link Config}, or an empty {@link Optional} if it's not found.
   * @throws org.raistlic.common.precondition.InvalidParameterException       when {@code key} is {@code null}.
   * @throws org.raistlic.common.config.exception.ConfigValueConvertException if a value is found
   *                                                                          for the {@code key} , but failed when being converted to {@code long} .
   */
  Optional<Long> getLong(String key);

  /**
   * The method queries the {@code float} value specified by the {@code key} .
   *
   * @param key   the key to search, cannot be {@code null}.
   * @param value the default value to be returned, in case {@code key} is not found.
   * @return the {@code float} value found in the {@link Config} by the {@code key} , or
   * the default {@code value} parameter in case {@code key} is not found.
   * @throws org.raistlic.common.precondition.InvalidParameterException       when {@code key} is {@code null}.
   * @throws org.raistlic.common.config.exception.ConfigValueConvertException if a value is found
   *                                                                          for the {@code key} , but failed when being converted to {@code float} .
   */
  default float getFloat(String key, float value) {

    return getFloat(key).orElse(value);
  }

  /**
   * The method queries the {@code float} value specified by the {@code key} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @return the {@code float} value found in the {@link Config}, or an empty {@link Optional} if it's not found.
   * @throws org.raistlic.common.precondition.InvalidParameterException       when {@code key} is {@code null}.
   * @throws org.raistlic.common.config.exception.ConfigValueConvertException if a value is found
   *                                                                          for the {@code key} , but failed when being converted to {@code float} .
   */
  Optional<Float> getFloat(String key);

  /**
   * The method queries the {@code double} value specified by the {@code key} .
   *
   * @param key   the key to search, cannot be {@code null}.
   * @param value the default value to be returned, in case {@code key} is not found.
   * @return the {@code double} value found in the {@link Config} by the {@code key} , or
   * the default {@code value} parameter in case {@code key} is not found.
   * @throws org.raistlic.common.precondition.InvalidParameterException       when {@code key} is {@code null}.
   * @throws org.raistlic.common.config.exception.ConfigValueConvertException if a value is found
   *                                                                          for the {@code key} , but failed when being converted to {@code double} .
   */
  default double getDouble(String key, double value) {

    return getDouble(key).orElse(value);
  }

  /**
   * The method queries the {@code double} value specified by the {@code key} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @return the {@code double} value found in the {@link Config}, or an empty {@link Optional} if it's not found.
   * @throws org.raistlic.common.precondition.InvalidParameterException       when {@code key} is {@code null}.
   * @throws org.raistlic.common.config.exception.ConfigValueConvertException if a value is found
   *                                                                          for the {@code key} , but failed when being converted to {@code double} .
   */
  Optional<Double> getDouble(String key);

  /**
   * The method queries the value (of type {@code <E>} specified by the {@code key} .
   *
   * <p>
   * The method is an overloading of {@link #getValue(String, org.raistlic.common.codec.Decoder, Object)} ,
   * and is an equivalent call as {@code getValue(key, decoder, null)} .
   *
   * @param key     the key to search, cannot be {@code null}.
   * @param decoder the decoder used to convert the value found from {@link String} to the required
   *                concrete type.
   * @param <E>     the concrete type of value to query
   * @return the value found and converted in the {@link Config} by the {@code key} , or
   * {@code null} in case {@code key} is not found.
   * @throws org.raistlic.common.precondition.InvalidParameterException       when {@code key} or
   *                                                                          {@code decoder} is {@code null}.
   * @throws org.raistlic.common.config.exception.ConfigValueConvertException if a value is found
   *                                                                          for the {@code key} , but failed when being converted to type {@code <E>} .
   */
  <E> Optional<E> getValue(String key, Decoder<? extends E, String> decoder);

  /**
   * The method queries the value (of type {@code <E>} specified by the {@code key} .
   *
   * @param key     the key to search, cannot be {@code null}.
   * @param decoder the decoder used to convert the value found from {@link String} to the required
   *                concrete type.
   * @param value   the default value to be returned in case {@code key} is not found.
   * @param <E>     the concrete type of value to query
   * @return the value found and converted in the {@link Config} by the {@code key} , or
   * the parameter {@code value} in case {@code key} is not found.
   * @throws org.raistlic.common.precondition.InvalidParameterException       when {@code key} or
   *                                                                          {@code decoder} is {@code null}.
   * @throws org.raistlic.common.config.exception.ConfigValueConvertException if a value is found
   *                                                                          for the {@code key} , but failed when being converted to type {@code <E>} .
   */
  default <E> E getValue(String key, Decoder<? extends E, String> decoder, E value) {

    return this.<E>getValue(key, decoder).orElse(value);
  }
}
