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
public class DefaultByteConverterTest extends BaseConverterTest{

  private Codec<Byte, String> converter = ConfigConverters.byteConverter();

  @Override
  Codec<?, String> getConverter() {

    return converter;
  }

  @Test(expected = ConfigValueConvertException.class)
  @Parameters(method = "invalidDecodeCases")
  @TestCaseName("decodeWithInvalidDests with '{0}")
  public void decodeWithInvalidDests(String dest) {

    converter.decode(dest);
  }

  private static List<Object[]> invalidDecodeCases() {

    List<Object[]> cases = new ArrayList<Object[]>();
    cases.add(new Object[] {""});
    cases.add(new Object[] {" "});
    cases.add(new Object[] {"asdf"});
    cases.add(new Object[] {"- 1"});
    cases.add(new Object[] {"+ 1"});
    cases.add(new Object[] {"1 323"});
    cases.add(new Object[] {"128"});
    cases.add(new Object[]{"-129"});
    return cases;
  }

  @Test
  @Parameters(method = "validDecodeCases")
  @TestCaseName("decodeExpected with '{0}', expected decode result: {1}")
  public void decodeExpected(String dest, Byte expected) {

    Byte actual = converter.decode(dest);
    assertThat(actual).isEqualTo(expected);
  }

  private static List<Object[]> validDecodeCases() {

    List<Object[]> cases = new ArrayList<Object[]>();
    cases.add(new Object[] {"1", (byte)1});
    cases.add(new Object[] {"123", (byte)123});
    cases.add(new Object[] {"   123", (byte)123});
    cases.add(new Object[] {"   -123", (byte)-123});
    cases.add(new Object[] {"123   ", (byte)123});
    cases.add(new Object[] {"-123   ", (byte)-123});
    cases.add(new Object[] {"1", (byte)1});
    cases.add(new Object[] {"-1", (byte)-1});
    cases.add(new Object[] {"0", (byte)0});
    cases.add(new Object[]{"-0", (byte)0});
    cases.add(new Object[]{"127", (byte)127});
    cases.add(new Object[]{"-128", (byte)-128});
    return cases;
  }

  @Test
  @Parameters(method = "validEncodeCases")
  @TestCaseName("encodeExpected with {0}, expected encode result: '{1}'")
  public void encodeExpected(Byte src, String expected) {

    String actual = converter.encode(src);
    assertThat(actual).isEqualTo(expected);
  }

  private static List<Object[]> validEncodeCases() {

    List<Object[]> cases = new ArrayList<Object[]>();
    cases.add(new Object[] { (byte)1, "1" });
    cases.add(new Object[] { (byte)123, "123" });
    cases.add(new Object[] { (byte)-1, "-1" });
    cases.add(new Object[] { (byte)-123, "-123" });
    cases.add(new Object[] { (byte)127, "127" });
    cases.add(new Object[] { (byte)-128, "-128" });
    cases.add(new Object[] { (byte)0, "0" });
    cases.add(new Object[] { (byte)-0, "0" });
    return cases;
  }
}
