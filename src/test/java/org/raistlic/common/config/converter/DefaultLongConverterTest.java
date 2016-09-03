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

import static org.raistlic.common.postcondition.Postcondition.assertThat;

/**
 * @author Lei Chen (2015-09-15)
 */
@SuppressWarnings("unused")
public class DefaultLongConverterTest extends BaseConverterTest {

  private Codec<Long, String> converter = ConfigConverters.longConverter();

  @Override
  Codec<?, String> getConverter() {

    return converter;
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
    cases.add(new Object[] {"- 123"});
    cases.add(new Object[] {"+ 123"});
    cases.add(new Object[] {"123a"});
    cases.add(new Object[] {"a123"});
    cases.add(new Object[] {"asdf"});
    cases.add(new Object[] {"0xb1d2"});
    cases.add(new Object[]{"34756398263498263492653456982472983462387562394679872983573847234"});
    return cases;
  }

  @Test
  @Parameters(method = "expectedDecodeCases")
  @TestCaseName("decodeExpected with '{0}', expected decode result: {1}")
  public void decodeExpected(String dest, Long expected) {

    Long actual = converter.decode(dest);
    assertThat(actual).isEqualTo(expected);
  }

  private static List<Object[]> expectedDecodeCases() {

    List<Object[]> cases = new ArrayList<Object[]>();
    cases.add(new Object[] {"1", 1L});
    cases.add(new Object[] {"123", 123L});
    cases.add(new Object[] {"0", 0L});
    cases.add(new Object[] {"-0", 0L});
    cases.add(new Object[] {"-1", -1L});
    cases.add(new Object[] {"1442320652", 1442320652L});
    cases.add(new Object[] {"123", 123L});
    cases.add(new Object[] {"   123", 123L});
    cases.add(new Object[] {"123   ", 123L});
    cases.add(new Object[] {"-123", -123L});
    cases.add(new Object[] {"   -123", -123L});
    cases.add(new Object[] {"-123   ", -123L});
    cases.add(new Object[] {"+123", 123L});
    cases.add(new Object[] {"   +123", 123L});
    cases.add(new Object[] {"+123   ", 123L});
    return cases;
  }

  @Test
  @Parameters(method = "expectedEncodeCases")
  @TestCaseName("encodeExpected with {0}, expected encode result: '{1}'")
  public void encodeExpected(Long src, String expected) {

    String actual = converter.encode(src);
    assertThat(actual).isEqualTo(expected);
  }

  private static List<Object[]> expectedEncodeCases() {

    List<Object[]> cases = new ArrayList<Object[]>();
    cases.add(new Object[] {1L, "1"});
    cases.add(new Object[] {123L, "123"});
    cases.add(new Object[] {-1L, "-1"});
    cases.add(new Object[] {0L, "0"});
    cases.add(new Object[] {-0L, "0"});
    cases.add(new Object[] {1442320652L, "1442320652"});
    cases.add(new Object[] {+1442320652L, "1442320652"});
    cases.add(new Object[] {-1442320652L, "-1442320652"});
    cases.add(new Object[] {+1L, "1"});
    return cases;
  }
}
