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
public class DefaultCharConverterTest extends BaseConverterTest {

  private Codec<Character, String> converter = ConfigConverters.charConverter();

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
    cases.add(new Object[] { "asdf" });
    cases.add(new Object[] { "12" });
    cases.add(new Object[] { "  " });
    cases.add(new Object[] { "1 " });
    cases.add(new Object[]{""});
    return cases;
  }

  private static List<Object[]> validCases() {

    List<Object[]> cases = new ArrayList<Object[]>();
    cases.add(new Object[] { "a", 'a' });
    cases.add(new Object[] { " ", ' ' });
    cases.add(new Object[] { "\t", '\t' });
    cases.add(new Object[] { "\n", '\n' });
    cases.add(new Object[] { "1", '1' });
    cases.add(new Object[] { "#", '#' });
    cases.add(new Object[] { "!", '!' });
    cases.add(new Object[] { ".", '.' });
    cases.add(new Object[]{"*", '*'});
    return cases;
  }

  @Test
  @Parameters(method = "validCases")
  @TestCaseName("decodeExpected with '{0}', expected decode result: {1}")
  public void decodeExpected(String dest, Character expected) {

    Character actual = converter.decode(dest);
    assertThat(actual).isEqualTo(expected);
  }

  @Test
  @Parameters(method = "validCases")
  @TestCaseName("isValidSrcExpected with src {1}")
  public void isValidSrcExpected(String unusedDest, Character src) {

    assertThat(converter.isValidSrc(src)).isTrue();
  }

  @Test
  @Parameters(method = "validCases")
  @TestCaseName("isValidSrcExpected with src {1}, expected dest '{0}'")
  public void encodeExpected(String expected, Character src) {

    String actual = converter.encode(src);
    assertThat(actual).isEqualTo(expected);
  }
}
