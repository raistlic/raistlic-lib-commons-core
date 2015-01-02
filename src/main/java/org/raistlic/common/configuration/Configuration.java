package org.raistlic.common.configuration;

import org.raistlic.common.Factory;
import org.raistlic.common.codec.Decoder;
import org.raistlic.common.codec.Encoder;

import java.util.Set;

/**
 * The interface abstracts the functionality of a configuration entity instance, which include
 * queries to multiple types of values using {@link String} key.
 *
 * @author lei.c
 * @since 2014-12-28
 */
public interface Configuration {

  /**
   * The method returns all the keys that the {@link Configuration} contains, as a read-only
   * {@link java.util.Set} .
   *
   * @return all the keys that the {@link Configuration} contains as a read-only {@link java.util.Set} ,
   *         any attempts to modify the returned {@link java.util.Set} may cause an exception.
   */
  Set<String> getKeys();

  /**
   * The method queries the {@link String} value specified by {@code key} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @return the {@link String} value specified by {@code key} in the {@link Configuration} ,
   *         or {@code null} if the {@code key} is not found.
   *
   * @throws java.lang.NullPointerException if {@code key} is {@code null}.
   */
  String getString(String key);

  /**
   * The method queries the {@link String} value specified by the {@code key} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @param value the default value to return in case {@code key} is not found.
   * @return the value in the {@link Configuration} specified by {@code key} , or the default
   *         {@code value} parameter in case {@code key} is not found.
   *
   * @throws java.lang.NullPointerException if {@code key} is {@code null}.
   */
  String getString(String key, String value);

  /**
   * The method queries the {@code byte} value specified by the {@code key} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @param value the default value to be returned, in case {@code key} is not found.
   * @return the {@code byte} value found in the {@link Configuration} by the {@code key} , or
   *         the default {@code value} parameter in case {@code key} is not found.
   *
   * @throws java.lang.NullPointerException if {@code key} is {@code null}.
   * @throws org.raistlic.common.configuration.ConfigurationValueConvertException if a value is found
   *         for the {@code key} , but failed when being converted to {@code byte} .
   */
  byte getByte(String key, byte value);

  /**
   * The method queries the {@code char} value specified by the {@code key} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @param value the default value to be returned, in case {@code key} is not found.
   * @return the {@code char} value found in the {@link Configuration} by the {@code key} , or
   *         the default {@code value} parameter in case {@code key} is not found.
   *
   * @throws java.lang.NullPointerException if {@code key} is {@code null}.
   * @throws org.raistlic.common.configuration.ConfigurationValueConvertException if a value is found
   *         for the {@code key} , but failed when being converted to {@code char} .
   */
  char getChar(String key, char value);

  /**
   * The method queries the {@code short} value specified by the {@code key} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @param value the default value to be returned, in case {@code key} is not found.
   * @return the {@code short} value found in the {@link Configuration} by the {@code key} , or
   *         the default {@code value} parameter in case {@code key} is not found.
   *
   * @throws java.lang.NullPointerException if {@code key} is {@code null}.
   * @throws org.raistlic.common.configuration.ConfigurationValueConvertException if a value is found
   *         for the {@code key} , but failed when being converted to {@code short} .
   */
  short getShort(String key, short value);

  /**
   * The method queries the {@code int} value specified by the {@code key} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @param value the default value to be returned, in case {@code key} is not found.
   * @return the {@code int} value found in the {@link Configuration} by the {@code key} , or
   *         the default {@code value} parameter in case {@code key} is not found.
   *
   * @throws java.lang.NullPointerException if {@code key} is {@code null}.
   * @throws org.raistlic.common.configuration.ConfigurationValueConvertException if a value is found
   *         for the {@code key} , but failed when being converted to {@code int} .
   */
  int getInt(String key, int value);

  /**
   * The method queries the {@code long} value specified by the {@code key} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @param value the default value to be returned, in case {@code key} is not found.
   * @return the {@code long} value found in the {@link Configuration} by the {@code key} , or
   *         the default {@code value} parameter in case {@code key} is not found.
   *
   * @throws java.lang.NullPointerException if {@code key} is {@code null}.
   * @throws org.raistlic.common.configuration.ConfigurationValueConvertException if a value is found
   *         for the {@code key} , but failed when being converted to {@code long} .
   */
  long getLong(String key, long value);

  /**
   * The method queries the {@code float} value specified by the {@code key} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @param value the default value to be returned, in case {@code key} is not found.
   * @return the {@code float} value found in the {@link Configuration} by the {@code key} , or
   *         the default {@code value} parameter in case {@code key} is not found.
   *
   * @throws java.lang.NullPointerException if {@code key} is {@code null}.
   * @throws org.raistlic.common.configuration.ConfigurationValueConvertException if a value is found
   *         for the {@code key} , but failed when being converted to {@code float} .
   */
  float getFloat(String key, float value);

  /**
   * The method queries the {@code double} value specified by the {@code key} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @param value the default value to be returned, in case {@code key} is not found.
   * @return the {@code double} value found in the {@link Configuration} by the {@code key} , or
   *         the default {@code value} parameter in case {@code key} is not found.
   *
   * @throws java.lang.NullPointerException if {@code key} is {@code null}.
   * @throws org.raistlic.common.configuration.ConfigurationValueConvertException if a value is found
   *         for the {@code key} , but failed when being converted to {@code double} .
   */
  double getDouble(String key, double value);

  /**
   * The method queries the value (of type {@code <E>} specified by the {@code key} .
   *
   * <p/>
   * The method is an overloading of {@link #getValue(String, org.raistlic.common.codec.Decoder, Object)} ,
   * and is an equivalent call as {@code getValue(key, decoder, null)} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @param decoder the decoder used to convert the value found from {@link String} to the required
   *                concrete type.
   * @param <E> the concrete type of value to query
   * @return the value found and converted in the {@link Configuration} by the {@code key} , or
   *         {@code null} in case {@code key} is not found.
   *
   * @throws java.lang.NullPointerException if {@code key} or {@code decoder} is {@code null}.
   * @throws org.raistlic.common.configuration.ConfigurationValueConvertException if a value is found
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
   * @return the value found and converted in the {@link Configuration} by the {@code key} , or
   *         the parameter {@code value} in case {@code key} is not found.
   *
   * @throws java.lang.NullPointerException if {@code key} or {@code decoder} is {@code null}.
   * @throws org.raistlic.common.configuration.ConfigurationValueConvertException if a value is found
   *         for the {@code key} , but failed when being converted to type {@code <E>} .
   */
  <E> E getValue(String key, Decoder<? extends E, String> decoder, E value);

  /**
   * The interface defines a builder of {@link Configuration}, which provides setter methods to
   * specify the key-value pairs to be included in the created {@link Configuration} instance.
   */
  public interface Builder extends Factory<Configuration> {

    /**
     * The method sets a {@link String} {@code value} to be mapped under the {@code key} .
     *
     * @param key the key used to map the {@code value}, cannot be {@code null}.
     * @param value the value to be mapped.
     *
     * @throws java.lang.NullPointerException if {@code key} is {@code null}.
     */
    void setString(String key, String value);

    /**
     * The method maps the {@code byte value} under the {@code key} .
     *
     * @param key the key used to map the {@code value}, cannot be {@code null}.
     * @param value the value to be mapped.
     *
     * @throws java.lang.NullPointerException if {@code key} is {@code null}.
     */
    void setByte(String key, byte value);

    /**
     * The method maps the {@code char value} under the {@code key} .
     *
     * @param key the key used to map the {@code value}, cannot be {@code null}.
     * @param value the value to be mapped.
     *
     * @throws java.lang.NullPointerException if {@code key} is {@code null}.
     */
    void setChar(String key, char value);

    /**
     * The method maps the {@code short value} under the {@code key} .
     *
     * @param key the key used to map the {@code value}, cannot be {@code null}.
     * @param value the value to be mapped.
     *
     * @throws java.lang.NullPointerException if {@code key} is {@code null}.
     */
    void setShort(String key, short value);

    /**
     * The method maps the {@code int value} under the {@code key} .
     *
     * @param key the key used to map the {@code value}, cannot be {@code null}.
     * @param value the value to be mapped.
     *
     * @throws java.lang.NullPointerException if {@code key} is {@code null}.
     */
    void setInt(String key, int value);

    /**
     * The method maps the {@code long value} under the {@code key} .
     *
     * @param key the key used to map the {@code value}, cannot be {@code null}.
     * @param value the value to be mapped.
     *
     * @throws java.lang.NullPointerException if {@code key} is {@code null}.
     */
    void setLong(String key, long value);

    /**
     * The method maps the {@code float value} under the {@code key} .
     *
     * @param key the key used to map the {@code value}, cannot be {@code null}.
     * @param value the value to be mapped.
     *
     * @throws java.lang.NullPointerException if {@code key} is {@code null}.
     */
    void setFloat(String key, float value);

    /**
     * The method maps the {@code double value} under the {@code key} .
     *
     * @param key the key used to map the {@code value}, cannot be {@code null}.
     * @param value the value to be mapped.
     *
     * @throws java.lang.NullPointerException if {@code key} is {@code null}.
     */
    void setDouble(String key, double value);

    /**
     * The method maps the {@code value} under the {@code key} .
     *
     * @param key the key used to map the {@code value}, cannot be {@code null}.
     * @param value the value to be mapped.
     * @param encoder the encoder used to convert {@code value} into a {@link String} , cannot be
     *                {@code null}.
     *
     * @throws java.lang.NullPointerException if {@code key} or {@code encoder} is {@code null}.
     */
    <E> void setValue(String key, E value, Encoder<? super E, String> encoder);
  }
}
