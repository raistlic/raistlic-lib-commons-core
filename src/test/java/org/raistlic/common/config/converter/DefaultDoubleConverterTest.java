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
 * @author Lei Chen (2015-09-17)
 */
@SuppressWarnings("unused")
public class DefaultDoubleConverterTest extends BaseConverterTest {

  private Codec<Double, String> converter = ConfigConverters.doubleConverter();

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
    cases.add(new Object[] {"asdf"});
    cases.add(new Object[] {"a123"});
    cases.add(new Object[] {"123a"});
    cases.add(new Object[] {"1.2.3.4"});
    cases.add(new Object[] {"2134j"});
    cases.add(new Object[] {"0x123F"});
    cases.add(new Object[] {"1.23.4.5"});
    cases.add(new Object[] {"1..4"});
    cases.add(new Object[] {"1.   43"});
    cases.add(new Object[] {"123 345"});
    return cases;
  }

  @Test
  @Parameters(method = "expectedDecodeCases")
  @TestCaseName("decodeExpected with '{0}'")
  public void decodeExpected(String dest, Double expected) {

    Double actual = converter.decode(dest);
    assertThat(actual).isEqualTo(expected);
  }

  private static List<Object[]> expectedDecodeCases() {

    List<Object[]> cases = new ArrayList<Object[]>();
    cases.add(new Object[] {"123.3245", 123.3245});
    cases.add(new Object[] {"1", (double)1});
    cases.add(new Object[] {"-1", (double)-1});
    cases.add(new Object[] {"1.00001", 1.00001});
    cases.add(new Object[] {"-1.00001", -1.00001});
    cases.add(new Object[] {"0", (double)0});
    cases.add(new Object[] {"+0", (double)0});
    cases.add(new Object[] {"-0", -0.0});
    cases.add(new Object[] {"    -0", -0.0});
    cases.add(new Object[]{"-0    ", -0.0});
    return cases;
  }

  @Test
  @Parameters(method = "expectedEncodeCases")
  @TestCaseName("encodeExpected with {0}, expected encode result: '{1}'")
  public void encodeExpected(Double src, String expected) {

    String actual = converter.encode(src);
    assertThat(actual).isEqualTo(expected);
  }

  private static List<Object[]> expectedEncodeCases() {

    List<Object[]> cases = new ArrayList<Object[]>();
    cases.add(new Object[] {1.0, "1.0"});
    cases.add(new Object[] {-1.0, "-1.0"});
    cases.add(new Object[] {1.234, "1.234"});
    cases.add(new Object[] {-1.234, "-1.234"});
    cases.add(new Object[] {0.0, "0.0"});
    cases.add(new Object[] {-0.0, "-0.0"});
    return cases;
  }
}
