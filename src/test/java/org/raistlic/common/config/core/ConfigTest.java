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

import junitparams.JUnitParamsRunner;
import junitparams.Parameters;
import junitparams.naming.TestCaseName;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.raistlic.common.codec.Codec;
import org.raistlic.common.codec.ValueConversionException;
import org.raistlic.common.config.exception.ConfigValueConvertException;
import org.raistlic.common.config.source.ConfigSourceFactory;
import org.raistlic.common.precondition.InvalidParameterException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.fest.assertions.api.Assertions.assertThat;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

/**
 * The unit test for common contract of the {@link Config} interface.
 *
 * @author Lei Chen (2015-09-11)
 */
@RunWith(JUnitParamsRunner.class)
@SuppressWarnings("unused")
public class ConfigTest {

  private static List<Object[]> getTestCases() {

    List<Object[]> testCases = new ArrayList<Object[]>();

    testCases.add(new Object[]{
        ConfigFactory.newMutableConfig().importFrom(FIXTURE_MAP),
        "with default mutable config implementation"
    });

    testCases.add(new Object[]{
        ConfigFactory.wrap(ConfigSourceFactory.immutableCopyOf(FIXTURE_MAP)),
        "with immutable config by wrapping immutable source"
    });

    testCases.add(new Object[]{
        ConfigFactory.newMutableConfig().importFrom(FIXTURE_MAP).get(),
        "with immutable config built by builder implementation"
    });

    return testCases;
  }

  private Codec<Object, String> valueConverter;

  @Before
  @SuppressWarnings("unchecked")
  public void setup() {

    valueConverter = mock(Codec.class);

    String fixtureValueString = FIXTURE_VALUE.toString();
    doReturn(false).when(valueConverter).isValidDest(anyString());
    doReturn(true).when(valueConverter).isValidDest(fixtureValueString);
    doThrow(new ValueConversionException("test exception"))
        .when(valueConverter).decode(anyString());
    doReturn(FIXTURE_VALUE)
        .when(valueConverter).decode(fixtureValueString);
  }

  @Test(expected = InvalidParameterException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("hasKeyWithNullKey {1}")
  public void hasKeyWithNullKey(Config config,  String description) {

    config.hasKey(null);
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasKeyNotFound {1}")
  public void hasKeyNotFound(Config config, String description) {

    boolean actual = config.hasKey("f6b0eb18-7d46-4759-bf2e-0de77641b013");
    assertThat(actual).isFalse();
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasKeyExpected {1}")
  public void hasKeyExpected(Config config, String description) {

    assertThat(config.hasKey(KEY_STRING)).isTrue();
    assertThat(config.hasKey(KEY_BOOLEAN)).isTrue();
    assertThat(config.hasKey(KEY_BYTE)).isTrue();
    assertThat(config.hasKey(KEY_CHAR)).isTrue();
    assertThat(config.hasKey(KEY_SHORT)).isTrue();
    assertThat(config.hasKey(KEY_INT)).isTrue();
    assertThat(config.hasKey(KEY_LONG)).isTrue();
    assertThat(config.hasKey(KEY_FLOAT)).isTrue();
    assertThat(config.hasKey(KEY_DOUBLE)).isTrue();
  }

  @Test(expected = InvalidParameterException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("getStringWithNullKey {1}")
  public void getStringWithNullKey(Config config, String description) {

    config.getString(null, "string");
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("getStringNotFound {1}")
  public void getStringNotFound(Config config, String description) {

    String key = "9eefaa6a-733c-440b-8670-3dd62fa0e5e6";
    String expected = "2cf38bd7-c4fe-435e-ad45-c44bf1237357";

    String actual = config.getString(key, expected);
    assertThat(actual).isEqualTo(expected);
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("getStringExpected {1}")
  public void getStringExpected(Config config, String description) {

    String key = KEY_STRING;
    String value = "f6b0eb18-7d46-4759-bf2e-0de77641b013";

    String actual = config.getString(key, value);
    assertThat(actual).isNotEqualTo(value);
    assertThat(actual).isEqualTo(FIXTURE_STRING);
  }

  @Test(expected = InvalidParameterException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("hasBooleanWithNullKey {1}")
  public void hasBooleanWithNullKey(Config config, String description) {

    config.hasBoolean(null);
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasBooleanNotFound {1}")
  public void hasBooleanNotFound(Config config, String description) {

    boolean actual = config.hasBoolean("f9c93342-c8a4-47cd-af44-9b5ac61e96ca");
    assertThat(actual).isFalse();
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasBooleanFoundButNotBoolean {1}")
  public void hasBooleanFoundButNotBoolean(Config config, String description) {

    boolean actual = config.hasBoolean(KEY_STRING);
    assertThat(actual).isFalse();
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasBooleanExpected {1}")
  public void hasBooleanExpected(Config config, String description) {

    boolean actual = config.hasBoolean(KEY_BOOLEAN);
    assertThat(actual).isTrue();
  }

  @Test(expected = InvalidParameterException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("getBooleanWithNullKey {1}")
  public void getBooleanWithNullKey(Config config, String description) {

    config.getBoolean(null, true);
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("getBooleanNotFound {1}")
  public void getBooleanNotFound(Config config, String description) {

    boolean actual = config.getBoolean("df18eed7-3ed9-428b-b1ae-bda63b1aa1d1", false);
    assertThat(actual).isFalse();
  }

  @Test(expected = ConfigValueConvertException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("getBooleanFoundButNotBoolean {1}")
  public void getBooleanFoundButNotBoolean(Config config, String description) {

    config.getBoolean(KEY_STRING, false);
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("getBooleanExpected {1}")
  public void getBooleanExpected(Config config, String description) {

    assertThat(config.getBoolean(KEY_BOOLEAN, false)).isTrue();
  }

  @Test(expected = InvalidParameterException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("hasByteWithNullKey {1}")
  public void hasByteWithNullKey(Config config, String description) {

    config.hasByte(null);
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasByteNotFound {1}")
  public void hasByteNotFound(Config config, String description) {

    boolean actual = config.hasByte("b2e5839f-6651-40ff-90e2-5c45737acd9b");
    assertThat(actual).isFalse();
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasByteFoundButNotByte {1}")
  public void hasByteFoundButNotByte(Config config, String description) {

    boolean actual = config.hasByte(KEY_STRING);
    assertThat(actual).isFalse();
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasByteExpected {1}")
  public void hasByteExpected(Config config, String description) {

    boolean actual = config.hasByte(KEY_BYTE);
    assertThat(actual).isTrue();
  }

  @Test(expected = InvalidParameterException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("getByteWithNullKey {1}")
  public void getByteWithNullKey(Config config, String description) {

    config.getByte(null, (byte) 1);
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("getByteNotFound {1}")
  public void getByteNotFound(Config config, String description) {

    byte actual = config.getByte("5262f9a3-c051-44e6-95d1-1dbc1233ead7", (byte) 1);
    assertThat(actual).isEqualTo((byte) 1);
  }

  @Test(expected = ConfigValueConvertException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("getByteFoundButNotByte {1}")
  public void getByteFoundButNotByte(Config config, String description) {

    config.getByte(KEY_STRING, (byte) 1);
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("getByteFoundButNotByte {1}")
  public void getByteExpected(Config config, String description) {

    byte actual = config.getByte(KEY_BYTE, (byte) 1);
    assertThat(actual).isNotEqualTo((byte) 1);
    assertThat(actual).isEqualTo(FIXTURE_BYTE);
  }

  @Test(expected = InvalidParameterException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("hasCharWithNullKey {1}")
  public void hasCharWithNullKey(Config config, String description) {

    config.hasChar(null);
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasCharWithNullKey {1}")
  public void hasCharNotFound(Config config, String description) {

    boolean actual = config.hasChar("2b56c1e0-b6ba-49cc-b299-3982c34dfc6e");
    assertThat(actual).isFalse();
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasCharFoundButNotChar {1}")
  public void hasCharFoundButNotChar(Config config, String description) {

    boolean actual = config.hasChar(KEY_STRING);
    assertThat(actual).isFalse();
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasCharFoundButNotChar {1}")
  public void hasCharExpected(Config config, String description) {

    boolean actual = config.hasChar(KEY_CHAR);
    assertThat(actual).isTrue();
  }

  @Test(expected = InvalidParameterException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("getCharWithNullKey {1}")
  public void getCharWithNullKey(Config config, String description) {

    config.getChar(null, '$');
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("getCharWithNullKey {1}")
  public void getCharNotFound(Config config, String description) {

    char actual = config.getChar("95a1e292-60e0-4154-a7d7-f719ac42b0b4", '$');
    assertThat(actual).isEqualTo('$');
  }

  @Test(expected = ConfigValueConvertException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("getCharFoundButNotChar {1}")
  public void getCharFoundButNotChar(Config config, String description) {

    config.getChar(KEY_STRING, '$');
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("getCharFoundButNotChar {1}")
  public void getCharExpected(Config config, String description) {

    char actual = config.getChar(KEY_CHAR, '$');
    assertThat(actual).isNotEqualTo('$');
    assertThat(actual).isEqualTo(FIXTURE_CHAR);
  }

  @Test(expected = InvalidParameterException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("hasShortWithNullKey {1}")
  public void hasShortWithNullKey(Config config, String description) {

    config.hasShort(null);
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasShortWithNullKey {1}")
  public void hasShortNotFound(Config config, String description) {

    boolean actual = config.hasShort("6d7a013a-cc08-4f5b-b8ad-c7f6350688f6");
    assertThat(actual).isFalse();
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasShortFoundButNotShort {1}")
  public void hasShortFoundButNotShort(Config config, String description) {

    boolean actual = config.hasShort(KEY_STRING);
    assertThat(actual).isFalse();
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasShortExpected {1}")
  public void hasShortExpected(Config config, String description) {

    boolean actual = config.hasShort(KEY_SHORT);
    assertThat(actual).isTrue();
  }

  @Test(expected = InvalidParameterException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("getShortWithNullKey {1}")
  public void getShortWithNullKey(Config config, String description) {

    config.getShort(null, (short) 1);
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("getShortNotFound {1}")
  public void getShortNotFound(Config config, String description) {

    String key = "f2f534ac-b138-44cf-9324-d21f2496dbe2";
    short value = (short) 1;

    short actual = config.getShort(key, value);
    assertThat(actual).isEqualTo(value);
  }

  @Test(expected = ConfigValueConvertException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("getShortNotFound {1}")
  public void getShortFoundButNotShort(Config config, String description) {

    config.getShort(KEY_STRING, (short) 1);
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("getShortNotFound {1}")
  public void getShortExpected(Config config, String description) {

    short value = (short) 1;
    short actual = config.getShort(KEY_SHORT, value);

    assertThat(actual).isNotEqualTo(value);
    assertThat(actual).isEqualTo(FIXTURE_SHORT);
  }

  @Test(expected = InvalidParameterException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("hasIntWithNullKey {1}")
  public void hasIntWithNullKey(Config config, String description) {

    config.hasInt(null);
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasIntNotFound {1}")
  public void hasIntNotFound(Config config, String description) {

    boolean actual = config.hasInt("2bb431af-29dc-472f-892c-f5257c42ba37");
    assertThat(actual).isFalse();
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasIntFoundButNotInt {1}")
  public void hasIntFoundButNotInt(Config config, String description) {

    boolean actual = config.hasInt(KEY_STRING);
    assertThat(actual).isFalse();
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasIntExpected {1}")
  public void hasIntExpected(Config config, String description) {

    boolean actual = config.hasInt(KEY_INT);
    assertThat(actual).isTrue();
  }

  @Test(expected = InvalidParameterException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("getIntWithNullKey {1}")
  public void getIntWithNullKey(Config config, String description) {

    config.getInt(null, 1);
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("getIntNotFound {1}")
  public void getIntNotFound(Config config, String description) {

    int actual = config.getInt("2e18a574-9334-4713-a4aa-b0bd76a1ee96", 1);
    assertThat(actual).isEqualTo(1);
  }

  @Test(expected = ConfigValueConvertException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("getIntFoundButNotInt {1}")
  public void getIntFoundButNotInt(Config config, String description) {

    config.getInt(KEY_STRING, 1);
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("getIntExpected {1}")
  public void getIntExpected(Config config, String description) {

    int actual = config.getInt(KEY_INT, 1);
    assertThat(actual).isNotEqualTo(1);
    assertThat(actual).isEqualTo(FIXTURE_INT);
  }

  @Test(expected = InvalidParameterException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("hasLongWithNullKey {1}")
  public void hasLongWithNullKey(Config config, String description) {

    config.hasLong(null);
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasLongNotFound {1}")
  public void hasLongNotFound(Config config, String description) {

    boolean actual = config.hasLong("cc04ad16-0448-4f64-b7c3-8b34cc8fe048");
    assertThat(actual).isFalse();
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasLongFoundButNotLong {1}")
  public void hasLongFoundButNotLong(Config config, String description) {

    boolean actual = config.hasLong(KEY_STRING);
    assertThat(actual).isFalse();
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasLongExpected {1}")
  public void hasLongExpected(Config config, String description) {

    boolean actual = config.hasLong(KEY_LONG);
    assertThat(actual).isTrue();
  }

  @Test(expected = InvalidParameterException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("getLongWithNullKey {1}")
  public void getLongWithNullKey(Config config, String description) {

    config.getLong(null, 1L);
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("getLongNotFound {1}")
  public void getLongNotFound(Config config, String description) {

    long actual = config.getLong("6130abf3-9e33-4353-8b6f-504300fe4ca0", 1L);
    assertThat(actual).isEqualTo(1L);
  }

  @Test(expected = ConfigValueConvertException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("getLongFoundButNotLong {1}")
  public void getLongFoundButNotLong(Config config, String description) {

    config.getLong(KEY_STRING, 1L);
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("getLongExpected {1}")
  public void getLongExpected(Config config, String description) {

    long actual = config.getLong(KEY_LONG, 1L);
    assertThat(actual).isNotEqualTo(1L);
    assertThat(actual).isEqualTo(FIXTURE_LONG);
  }

  @Test(expected = InvalidParameterException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("hasFloatWithNullKey {1}")
  public void hasFloatWithNullKey(Config config, String description) {

    config.hasFloat(null);
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasFloatNotFound {1}")
  public void hasFloatNotFound(Config config, String description) {

    boolean actual = config.hasFloat("40af8c4e-7fc7-46be-ab5a-1a8aee46d713");
    assertThat(actual).isFalse();
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasFloatFoundButNotFloat {1}")
  public void hasFloatFoundButNotFloat(Config config, String description) {

    boolean actual = config.hasFloat(KEY_STRING);
    assertThat(actual).isFalse();
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasFloatExpected {1}")
  public void hasFloatExpected(Config config, String description) {

    boolean actual = config.hasFloat(KEY_FLOAT);
    assertThat(actual).isTrue();
  }

  @Test(expected = InvalidParameterException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("getFloatWithNullKey {1}")
  public void getFloatWithNullKey(Config config, String description) {

    config.getFloat(null, 1F);
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("getFloatNotFound {1}")
  public void getFloatNotFound(Config config, String description) {

    float actual = config.getFloat("6c598679-51a8-4b6a-a814-24e82d91eb8d", 1F);
    assertThat(actual).isEqualTo(1F);
  }

  @Test(expected = ConfigValueConvertException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("getFloatFoundButNotFloat {1}")
  public void getFloatFoundButNotFloat(Config config, String description) {

    config.getFloat(KEY_STRING, 1F);
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("getFloatExpected {1}")
  public void getFloatExpected(Config config, String description) {

    float actual = config.getFloat(KEY_FLOAT, 1F);
    assertThat(actual).isNotEqualTo(1F);
    assertThat(actual).isEqualTo(FIXTURE_FLOAT);
  }

  @Test(expected = InvalidParameterException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("hasDoubleWithNullKey {1}")
  public void hasDoubleWithNullKey(Config config, String description) {

    config.hasDouble(null);
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasDoubleNotFound {1}")
  public void hasDoubleNotFound(Config config, String description) {

    boolean actual = config.hasDouble("05656f6a-cebb-4e84-b4f0-993c92e022d5");
    assertThat(actual).isFalse();
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasDoubleFoundButNotDouble {1}")
  public void hasDoubleFoundButNotDouble(Config config, String description) {

    boolean actual = config.hasDouble(KEY_STRING);
    assertThat(actual).isFalse();
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasDoubleExpected {1}")
  public void hasDoubleExpected(Config config, String description) {

    boolean actual = config.hasDouble(KEY_DOUBLE);
    assertThat(actual).isTrue();
  }

  @Test(expected = InvalidParameterException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("getDoubleWithNullKey {1}")
  public void getDoubleWithNullKey(Config config, String description) {

    config.getDouble(null, 1.0);
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("getDoubleNotFound {1}")
  public void getDoubleNotFound(Config config, String description) {

    double actual = config.getDouble("58a04a0a-088c-4a0f-a014-46c90c53437b", 1.0);
    assertThat(actual).isEqualTo(1.0);
  }

  @Test(expected = ConfigValueConvertException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("getDoubleFoundButNotDouble {1}")
  public void getDoubleFoundButNotDouble(Config config, String description) {

    config.getDouble(KEY_STRING, 1.0);
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("getDoubleExpected {1}")
  public void getDoubleExpected(Config config, String description) {

    double actual = config.getDouble(KEY_DOUBLE, 1.0);
    assertThat(actual).isNotEqualTo(1.0);
    assertThat(actual).isEqualTo(FIXTURE_DOUBLE);
  }

  @Test(expected = InvalidParameterException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("hasValueWithNullKey {1}")
  public void hasValueWithNullKey(Config config, String description) {

    config.hasValue(null, valueConverter);
  }

  @Test(expected = InvalidParameterException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("hasValueWithNullDecoder {1}")
  public void hasValueWithNullDecoder(Config config, String description) {

    config.hasValue("92655b7f-90ed-49fb-b2d6-de44a07dd298", null);
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasValueNotFound {1}")
  public void hasValueNotFound(Config config, String description) {

    boolean actual = config.hasValue("7f692936-b41b-4761-9220-08d796dc685d", valueConverter);
    assertThat(actual).isFalse();
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasValueFoundButCannotConvert {1}")
  public void hasValueFoundButCannotConvert(Config config, String description) {

    boolean actual = config.hasValue(KEY_STRING, valueConverter);
    verify(valueConverter).isValidDest(FIXTURE_STRING);
    assertThat(actual).isFalse();
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("hasValueExpected {1}")
  public void hasValueExpected(Config config, String description) {

    boolean actual = config.hasValue(KEY_VALUE, valueConverter);
    verify(valueConverter).isValidDest(FIXTURE_VALUE.toString());
    assertThat(actual).isTrue();
  }

  @Test(expected = InvalidParameterException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("getValueWithNullKey {1}")
  public void getValueWithNullKey(Config config, String description) {

    config.getValue(null, valueConverter);
  }

  @Test(expected = InvalidParameterException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("getValueWithNullDecoder {1}")
  public void getValueWithNullDecoder(Config config, String description) {

    String key = "8dd11f23-3e41-4823-9634-dfe1b966d0ab";
    config.getValue(key, null);
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("getValueNotFound {1}")
  public void getValueNotFound(Config config, String description) {

    String key = "140855fb-8e07-494b-94df-43050888030f";
    Object actual = config.getValue(key, valueConverter);
    assertThat(actual).isNull();

    Object value = new Object();
    actual = config.getValue(key, valueConverter, value);
    assertThat(actual).isEqualTo(value);
  }

  @Test(expected = ConfigValueConvertException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("getValueFoundButCannotConvert {1}")
  public void getValueFoundButCannotConvert(Config config, String description) {

    config.getValue(KEY_STRING, valueConverter);
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("getValueExpected {1}")
  public void getValueExpected(Config config, String description) {

    Object actual = config.getValue(KEY_VALUE, valueConverter);
    assertThat(actual).isEqualTo(FIXTURE_VALUE);
  }

  private static Map<String, String> createFixtureMap() {

    Map<String, String> map = new HashMap<String, String>();
    map.put(KEY_BYTE, String.valueOf(FIXTURE_BYTE));
    map.put(KEY_CHAR, String.valueOf(FIXTURE_CHAR));
    map.put(KEY_INT, String.valueOf(FIXTURE_INT));
    map.put(KEY_SHORT, String.valueOf(FIXTURE_SHORT));
    map.put(KEY_LONG, String.valueOf(FIXTURE_LONG));
    map.put(KEY_FLOAT, String.valueOf(FIXTURE_FLOAT));
    map.put(KEY_DOUBLE, String.valueOf(FIXTURE_DOUBLE));
    map.put(KEY_BOOLEAN, String.valueOf(FIXTURE_BOOLEAN));
    map.put(KEY_STRING, String.valueOf(FIXTURE_STRING));
    map.put(KEY_VALUE, String.valueOf(FIXTURE_VALUE));
    return map;
  }

  private static final byte FIXTURE_BYTE = (byte) 12;

  private static final char FIXTURE_CHAR = 'c';

  private static final short FIXTURE_SHORT = -325;

  private static final int FIXTURE_INT = 12345;

  private static final long FIXTURE_LONG = -234523458923234L;

  private static final float FIXTURE_FLOAT = 123.4F;

  private static final double FIXTURE_DOUBLE = 234.1;

  private static final boolean FIXTURE_BOOLEAN = true;

  private static final String FIXTURE_STRING = "8c41ad9c-efb7-11e4-90ec-1681e6b88ec1";

  private static final Object FIXTURE_VALUE = new Object();

  private static final String KEY_STRING = "string.value";

  private static final String KEY_BOOLEAN = "boolean.value";

  private static final String KEY_BYTE = "byte.value";

  private static final String KEY_CHAR = "char.value";

  private static final String KEY_SHORT = "short.value";

  private static final String KEY_INT = "int.value";

  private static final String KEY_LONG = "long.value";

  private static final String KEY_FLOAT = "float.value";

  private static final String KEY_DOUBLE = "double.value";

  private static final String KEY_VALUE = "value.value";

  private static final Map<String, String> FIXTURE_MAP = createFixtureMap();
}
