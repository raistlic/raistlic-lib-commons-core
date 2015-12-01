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
   * @param key the key to search, cannot be {@code null}.
   * @param value the default value to return in case {@code key} is not found.
   * @return the value in the {@link Config} specified by {@code key} , or the default
   *         {@code value} parameter in case {@code key} is not found.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} is {@code null}.
   */
  String getString(String key, String value);

  /**
   * The method queries whether the {@link Config} has a {@code boolean} value for the specified
   * {@code key}.
   *
   * @param key the key to query, cannot be {@code null}.
   * @return {@code true} only when the {@link Config} has a value for the {@code key} , and that the
   *         value is a valid {@code boolean} representation.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} is {@code null}.
   */
  boolean hasBoolean(String key);

  /**
   * The method queries the {@code boolean} value specified by the {@code key} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @param value the default value to return in case {@code key} is not found.
   * @return the value mapped for {@code key} in the {@link Config}, or the default
   *         {@code value} parameter in case {@code key} is not found.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} is {@code null}.
   * @throws org.raistlic.common.config.exception.ConfigValueConvertException when there is a value
   *         found for the {@code key} , but error occur when trying to convert it to a {@code boolean}
   *         value. See also {@link #hasKey(String)} and {@link #hasBoolean(String)} .
   */
  boolean getBoolean(String key, boolean value);

  /**
   * The method queries whether the {@link Config} has a {@code byte} value for the specified
   * {@code key}.
   *
   * @param key the key to query, cannot be {@code null}.
   * @return {@code true} only when the {@link Config} has a value for the {@code key} , and that the
   *         value is a valid {@code byte} representation.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} is {@code null}.
   */
  boolean hasByte(String key);

  /**
   * The method queries the {@code byte} value specified by the {@code key} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @param value the default value to be returned, in case {@code key} is not found.
   * @return the {@code byte} value found in the {@link Config} by the {@code key} , or
   *         the default {@code value} parameter in case {@code key} is not found.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} is {@code null}.
   * @throws org.raistlic.common.config.exception.ConfigValueConvertException if a value is found
   *         for the {@code key} , but failed when being converted to {@code byte} .
   */
  byte getByte(String key, byte value);

  /**
   * The method queries whether the {@link Config} has a {@code char} value for the specified
   * {@code key}.
   *
   * @param key the key to query, cannot be {@code null}.
   * @return {@code true} only when the {@link Config} has a value for the {@code key} , and that the
   *         value is a valid {@code char} representation.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} is {@code null}.
   */
  boolean hasChar(String key);

  /**
   * The method queries the {@code char} value specified by the {@code key} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @param value the default value to be returned, in case {@code key} is not found.
   * @return the {@code char} value found in the {@link Config} by the {@code key} , or
   *         the default {@code value} parameter in case {@code key} is not found.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} is {@code null}.
   * @throws org.raistlic.common.config.exception.ConfigValueConvertException if a value is found
   *         for the {@code key} , but failed when being converted to {@code char} .
   */
  char getChar(String key, char value);

  /**
   * The method queries whether the {@link Config} has a {@code short} value for the specified
   * {@code key}.
   *
   * @param key the key to query, cannot be {@code null}.
   * @return {@code true} only when the {@link Config} has a value for the {@code key} , and that the
   *         value is a valid {@code short} representation.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} is {@code null}.
   */
  boolean hasShort(String key);

  /**
   * The method queries the {@code short} value specified by the {@code key} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @param value the default value to be returned, in case {@code key} is not found.
   * @return the {@code short} value found in the {@link Config} by the {@code key} , or
   *         the default {@code value} parameter in case {@code key} is not found.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} is {@code null}.
   * @throws org.raistlic.common.config.exception.ConfigValueConvertException if a value is found
   *         for the {@code key} , but failed when being converted to {@code short} .
   */
  short getShort(String key, short value);

  /**
   * The method queries whether the {@link Config} has a {@code int} value for the specified
   * {@code key}.
   *
   * @param key the key to query, cannot be {@code null}.
   * @return {@code true} only when the {@link Config} has a value for the {@code key} , and that the
   *         value is a valid {@code int} representation.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} is {@code null}.
   */
  boolean hasInt(String key);

  /**
   * The method queries the {@code int} value specified by the {@code key} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @param value the default value to be returned, in case {@code key} is not found.
   * @return the {@code int} value found in the {@link Config} by the {@code key} , or
   *         the default {@code value} parameter in case {@code key} is not found.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} is {@code null}.
   * @throws org.raistlic.common.config.exception.ConfigValueConvertException if a value is found
   *         for the {@code key} , but failed when being converted to {@code int} .
   */
  int getInt(String key, int value);

  /**
   * The method queries whether the {@link Config} has a {@code long} value for the specified
   * {@code key}.
   *
   * @param key the key to query, cannot be {@code null}.
   * @return {@code true} only when the {@link Config} has a value for the {@code key} , and that the
   *         value is a valid {@code long} representation.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} is {@code null}.
   */
  boolean hasLong(String key);

  /**
   * The method queries the {@code long} value specified by the {@code key} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @param value the default value to be returned, in case {@code key} is not found.
   * @return the {@code long} value found in the {@link Config} by the {@code key} , or
   *         the default {@code value} parameter in case {@code key} is not found.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} is {@code null}.
   * @throws org.raistlic.common.config.exception.ConfigValueConvertException if a value is found
   *         for the {@code key} , but failed when being converted to {@code long} .
   */
  long getLong(String key, long value);

  /**
   * The method queries whether the {@link Config} has a {@code float} value for the specified
   * {@code key}.
   *
   * @param key the key to query, cannot be {@code null}.
   * @return {@code true} only when the {@link Config} has a value for the {@code key} , and that the
   *         value is a valid {@code float} representation.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} is {@code null}.
   */
  boolean hasFloat(String key);

  /**
   * The method queries the {@code float} value specified by the {@code key} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @param value the default value to be returned, in case {@code key} is not found.
   * @return the {@code float} value found in the {@link Config} by the {@code key} , or
   *         the default {@code value} parameter in case {@code key} is not found.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} is {@code null}.
   * @throws org.raistlic.common.config.exception.ConfigValueConvertException if a value is found
   *         for the {@code key} , but failed when being converted to {@code float} .
   */
  float getFloat(String key, float value);

  /**
   * The method queries whether the {@link Config} has a {@code double} value for the specified
   * {@code key}.
   *
   * @param key the key to query, cannot be {@code null}.
   * @return {@code true} only when the {@link Config} has a value for the {@code key} , and that the
   *         value is a valid {@code double} representation.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} is {@code null}.
   */
  boolean hasDouble(String key);

  /**
   * The method queries the {@code double} value specified by the {@code key} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @param value the default value to be returned, in case {@code key} is not found.
   * @return the {@code double} value found in the {@link Config} by the {@code key} , or
   *         the default {@code value} parameter in case {@code key} is not found.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} is {@code null}.
   * @throws org.raistlic.common.config.exception.ConfigValueConvertException if a value is found
   *         for the {@code key} , but failed when being converted to {@code double} .
   */
  double getDouble(String key, double value);

  /**
   * The method queries whether the {@link Config} has a value for the specified {@code key}, that
   * can be safely converted by the specified {@code decoder}.
   *
   * @param key the key to query, cannot be {@code null}.
   * @param decoder the decoder to check the value if exists, cannot be {@code null}.
   * @return {@code true} only when the {@link Config} has a value for the {@code key}, that can be
   *         safely converted by the specified {@code decoder}.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} or
   *         {@code decoder} is {@code null}.
   */
  boolean hasValue(String key, Decoder<?, String> decoder);

  /**
   * The method queries the value (of type {@code <E>} specified by the {@code key} .
   *
   * <p>
   * The method is an overloading of {@link #getValue(String, org.raistlic.common.codec.Decoder, Object)} ,
   * and is an equivalent call as {@code getValue(key, decoder, null)} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @param decoder the decoder used to convert the value found from {@link String} to the required
   *                concrete type.
   * @param <E> the concrete type of value to query
   * @return the value found and converted in the {@link Config} by the {@code key} , or
   *         {@code null} in case {@code key} is not found.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} or
   *         {@code decoder} is {@code null}.
   * @throws org.raistlic.common.config.exception.ConfigValueConvertException if a value is found
   *         for the {@code key} , but failed when being converted to type {@code <E>} .
   */
  <E> E getValue(String key, Decoder<? extends E, String> decoder);

  /**
   * The method queries the value (of type {@code <E>} specified by the {@code key} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @param decoder the decoder used to convert the value found from {@link String} to the required
   *                concrete type.
   * @param value the default value to be returned in case {@code key} is not found.
   * @param <E> the concrete type of value to query
   * @return the value found and converted in the {@link Config} by the {@code key} , or
   *         the parameter {@code value} in case {@code key} is not found.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} or
   *         {@code decoder} is {@code null}.
   * @throws org.raistlic.common.config.exception.ConfigValueConvertException if a value is found
   *         for the {@code key} , but failed when being converted to type {@code <E>} .
   */
  <E> E getValue(String key, Decoder<? extends E, String> decoder, E value);
}
