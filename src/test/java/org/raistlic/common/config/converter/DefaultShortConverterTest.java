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

import junitparams.Parameters;
import junitparams.naming.TestCaseName;
import org.junit.Test;
import org.raistlic.common.codec.Codec;
import org.raistlic.common.config.exception.ConfigValueConvertException;

import java.util.ArrayList;
import java.util.List;

import static org.fest.assertions.api.Assertions.assertThat;

/**
 * @author Lei Chen (2015-09-15)
 */
@SuppressWarnings("unused")
public class DefaultShortConverterTest extends BaseConverterTest {

  private Codec<Short, String> converter = ConfigConverters.shortConverter();

  @Override
  Codec<?, String> getConverter() {

    return converter;
  }

  @Test
  @Parameters(method = "invalidDecodeCases")
  @TestCaseName("isValidDestWithInvalidDests with '{0}'")
  public void isValidDestWithInvalidDests(String dest) {

    assertThat(converter.isValidDest(dest)).isFalse();
  }

  @Test(expected = ConfigValueConvertException.class)
  @Parameters(method = "invalidDecodeCases")
  @TestCaseName("decodeWithInvalidDests with '{0}'")
  public void decodeWithInvalidDests(String dest) {

    converter.decode(dest);
  }

  private static List<Object[]> invalidDecodeCases() {

    List<Object[]> cases = new ArrayList<Object[]>();
    cases.add(new Object[] {""});
    cases.add(new Object[] {" "});
    cases.add(new Object[] {"asdf"});
    cases.add(new Object[] {"a1"});
    cases.add(new Object[] {"0xa1"});
    cases.add(new Object[] {"12300000000"});
    cases.add(new Object[] {"234 23"});
    cases.add(new Object[] {"--1"});
    cases.add(new Object[] {"+-1"});
    cases.add(new Object[] {"++1"});
    cases.add(new Object[] {"-+1"});
    return cases;
  }

  @Test
  @Parameters(method = "expectedDecodeCases")
  @TestCaseName("isValidDestExpected with '{0}'")
  public void isValidDestExpected(String dest, Short unused) {

    assertThat(converter.isValidDest(dest)).isTrue();
  }

  @Test
  @Parameters(method = "expectedDecodeCases")
  @TestCaseName("decodeExpected with '{0}', expected decode result: {1}")
  public void decodeExpected(String dest, Short expected) {

    Short actual = converter.decode(dest);
    assertThat(actual).isEqualTo(expected);
  }

  private static List<Object[]> expectedDecodeCases() {

    List<Object[]> cases = new ArrayList<Object[]>();
    cases.add(new Object[] {"1", (short)1});
    cases.add(new Object[] {"-1", (short)-1});
    cases.add(new Object[] {"123", (short)123});
    cases.add(new Object[] {"    123", (short)123});
    cases.add(new Object[] {"123    ", (short)123});
    cases.add(new Object[] {"-123", (short)-123});
    cases.add(new Object[] {"    -123", (short)-123});
    cases.add(new Object[] {"-123    ", (short)-123});
    cases.add(new Object[] {"+123", (short)123});
    cases.add(new Object[] {"    +123", (short)123});
    cases.add(new Object[] {"+123    ", (short)123});
    cases.add(new Object[] {"0", (short)0});
    cases.add(new Object[] {"-0", (short)0});
    return cases;
  }

  @Test
  @Parameters(method = "expectedEncodeCases")
  @TestCaseName("isValidSrcExpected with {0}")
  public void isValidSrcExpected(Short src, String unused) {

    assertThat(converter.isValidSrc(src)).isTrue();
  }

  @Test
  @Parameters(method = "expectedEncodeCases")
  @TestCaseName("encodeExpected with {0}, expected encode result: '{1}'")
  public void encodeExpected(Short src, String expected) {

    String actual = converter.encode(src);
    assertThat(actual).isEqualTo(expected);
  }

  private static List<Object[]> expectedEncodeCases() {

    List<Object[]> cases = new ArrayList<Object[]>();
    cases.add(new Object[] {(short)1, "1"});
    cases.add(new Object[] {(short)-1, "-1"});
    cases.add(new Object[] {(short)0, "0"});
    cases.add(new Object[] {(short)-0, "0"});
    cases.add(new Object[] {(short)123, "123"});
    cases.add(new Object[] {(short)-123, "-123"});
    cases.add(new Object[] {(short)+123, "123"});
    return cases;
  }
}
