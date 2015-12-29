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

package org.raistlic.common.codec;

import org.raistlic.common.precondition.InvalidParameterException;
import org.raistlic.common.precondition.Precondition;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * A collection of static factory methods that export commonly used {@link Deserializer} instances.
 *
 * @author Lei Chen
 * @since 1.4
 */
public final class Deserializers {

  /**
   * The method exports a {@link Deserializer} instance that simply returns what ever {@link String}
   * target value passed in, given that the target is not {@code null} (or the deserializer will
   * throw an {@link InvalidParameterException} ).
   *
   * @return the stateless, singleton {@link Deserializer} instance.
   */
  public static Deserializer<String> getStringDeserializer() {

    return StringDeserializer.INSTANCE;
  }

  /**
   * The method exports a {@link Deserializer} instance that decodes a {@link String} target value
   * into a {@link Boolean} , as long as it's a valid target (or the deserializer will throw an
   * {@link InvalidParameterException} ).
   *
   * @return the stateless, singleton {@link Deserializer} instance.
   */
  public static Deserializer<Boolean> getBooleanDeserializer() {

    return BooleanDeserializer.INSTANCE;
  }

  public static Deserializer<Byte> getByteDeserializer() {

    return ByteDeserializer.INSTANCE;
  }

  public static Deserializer<Character> getCharacterDeserializer() {

    return CharacterDeserializer.INSTANCE;
  }

  public static Deserializer<Short> getShortDeserializer() {

    return ShortDeserializer.INSTANCE;
  }

  public static Deserializer<Integer> getIntegerDeserializer() {

    return IntegerDeserializer.INSTANCE;
  }

  public static Deserializer<Long> getLongDeserializer() {

    return LongDeserializer.INSTANCE;
  }

  public static Deserializer<Float> getFloatDeSerializer() {

    return FloatDeserializer.INSTANCE;
  }

  public static Deserializer<Double> getDoubleDeserializer() {

    return DoubleDeserializer.INSTANCE;
  }

  public static Deserializer<BigInteger> getBigIntegerDeserializer() {

    return BigIntegerDeserializer.INSTANCE;
  }

  public static Deserializer<BigDecimal> getBigDecimalDeserializer() {

    return BigDecimalDeserializer.INSTANCE;
  }

  private enum StringDeserializer implements Deserializer<String> {

    INSTANCE;

    @Override
    public String decode(String target) throws InvalidParameterException, ValueConversionException {

      Precondition.param(target, "target").notNull();

      return target;
    }
  }

  private enum BooleanDeserializer implements Deserializer<Boolean> {

    INSTANCE;

    @Override
    public Boolean decode(String target) throws InvalidParameterException, ValueConversionException {

      Precondition.param(target, "target").notNull();

      String converted = target.trim().toLowerCase();
      if (converted.equals("true")) {
        return true;
      }
      else if (converted.equals("false")) {
        return false;
      }
      else {
        throw new ValueConversionException("Cannot convert String value to Boolean: '" + target + "'");
      }
    }
  }

  private enum ByteDeserializer implements Deserializer<Byte> {

    INSTANCE;

    @Override
    public Byte decode(String target) throws InvalidParameterException, ValueConversionException {

      Precondition.param(target, "target").notNull();

      try {
        return Byte.valueOf(target);
      }
      catch (Exception ex) {
        throw new ValueConversionException(ex);
      }
    }
  }

  private enum CharacterDeserializer implements Deserializer<Character> {

    INSTANCE;

    @Override
    public Character decode(String target) throws InvalidParameterException, ValueConversionException {

      Precondition.param(target, "target").notNull();

      if (target.length() != 1) {
        throw new ValueConversionException("Cannot convert the target String to char: '" + target + "'");
      }
      return target.charAt(0);
    }
  }

  private enum ShortDeserializer implements Deserializer<Short> {

    INSTANCE;

    @Override
    public Short decode(String target) throws InvalidParameterException, ValueConversionException {

      Precondition.param(target, "target").notNull();

      try {
        return Short.valueOf(target);
      }
      catch (Exception ex) {
        throw new ValueConversionException(ex);
      }
    }
  }

  private enum IntegerDeserializer implements Deserializer<Integer> {

    INSTANCE;

    @Override
    public Integer decode(String target) {

      Precondition.param(target, "target").notNull();

      try {
        return Integer.valueOf(target);
      }
      catch (Exception ex) {
        throw new ValueConversionException(ex);
      }
    }
  }

  private enum LongDeserializer implements Deserializer<Long> {

    INSTANCE;

    @Override
    public Long decode(String target) throws InvalidParameterException, ValueConversionException {

      Precondition.param(target, "target").notNull();

      try {
        return Long.valueOf(target);
      }
      catch (Exception ex) {
        throw new ValueConversionException(ex);
      }
    }
  }

  private enum FloatDeserializer implements Deserializer<Float> {

    INSTANCE;

    @Override
    public Float decode(String target) throws InvalidParameterException, ValueConversionException {

      Precondition.param(target, "target").notNull();

      try {
        return Float.valueOf(target);
      }
      catch (Exception ex) {
        throw new ValueConversionException(ex);
      }
    }
  }

  private enum DoubleDeserializer implements Deserializer<Double> {

    INSTANCE;

    @Override
    public Double decode(String target) throws InvalidParameterException, ValueConversionException {

      Precondition.param(target, "target").notNull();

      try {
        return Double.valueOf(target);
      }
      catch (Exception ex) {
        throw new ValueConversionException(ex);
      }
    }
  }

  private enum BigIntegerDeserializer implements Deserializer<BigInteger> {

    INSTANCE;

    @Override
    public BigInteger decode(String target) throws InvalidParameterException, ValueConversionException {

      Precondition.param(target, "target").notNull();

      try {
        return new BigInteger(target);
      }
      catch (Exception ex) {
        throw new ValueConversionException(ex);
      }
    }
  }

  private enum BigDecimalDeserializer implements Deserializer<BigDecimal> {

    INSTANCE;

    @Override
    public BigDecimal decode(String target) throws InvalidParameterException, ValueConversionException {

      Precondition.param(target, "target").notNull();

      try {
        return new BigDecimal(target);
      }
      catch (Exception ex) {
        throw new ValueConversionException(ex);
      }
    }
  }
}
