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

import java.util.Arrays;
import java.util.List;

import static org.fest.assertions.api.Assertions.assertThat;

/**
 * @author Lei Chen (2015-09-14)
 */
@SuppressWarnings("unused")
public class DefaultBooleanConverterTest extends BaseConverterTest {

  private Codec<Boolean, String> converter = ConfigConverters.booleanConverter();

  @Override
  Codec<?, String> getConverter() {

    return converter;
  }

  @Test(expected = ConfigValueConvertException.class)
  @Parameters(method = "invalidCases")
  @TestCaseName("decodeWithInvalidDest with '{0}'")
  public void decodeWithInvalidDest(String dest) {

    converter.decode(dest);
  }

  private static List<Object[]> invalidCases() {

    return Arrays.asList(

        new Object[] { "" },
        new Object[] { "asdf" },
        new Object[] { "tru" },
        new Object[] { "123" },
        new Object[] { "false123" },
        new Object[] { "0" },
        new Object[] { "1" },
        new Object[] { "   " }
    );
  }

  @Test
  @Parameters(method = "validCases")
  @TestCaseName("decodeExpected with '{0}' and expected result {1}")
  public void decodeExpected(String dest, Boolean expected) {

    assertThat(converter.decode(dest)).isEqualTo(expected);
  }

  private static List<Object[]> validCases() {

    return Arrays.asList(
        new Object[] {"true", Boolean.TRUE},
        new Object[] {"true ", Boolean.TRUE},
        new Object[] {" true", Boolean.TRUE},
        new Object[] {"TRUE", Boolean.TRUE},
        new Object[] {" TRUE ", Boolean.TRUE},
        new Object[] {"True", Boolean.TRUE},
        new Object[] {" True   ", Boolean.TRUE},

        new Object[] {"false", Boolean.FALSE},
        new Object[] {"false ", Boolean.FALSE},
        new Object[] {" false", Boolean.FALSE},
        new Object[] {"FALSE", Boolean.FALSE},
        new Object[] {" FALSE ", Boolean.FALSE},
        new Object[] {"False", Boolean.FALSE},
        new Object[] {" False   ", Boolean.FALSE}
    );
  }

  @Test
  public void encodeExpected() {

    assertThat(converter.encode(true)).isEqualTo("true");
    assertThat(converter.encode(false)).isEqualTo("false");
  }
}
