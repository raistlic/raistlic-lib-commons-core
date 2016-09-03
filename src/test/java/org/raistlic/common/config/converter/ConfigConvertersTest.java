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

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import org.raistlic.common.codec.Codec;

import static org.raistlic.common.postcondition.Postcondition.assertThat;

/**
 * @author Lei Chen (2015-09-14)
 */
@RunWith(JUnit4.class)
public class ConfigConvertersTest {

  @Test
  public void booleanConverterNotNull() {

    assertThat(ConfigConverters.booleanConverter()).isNotNull();
  }

  @Test
  public void booleanConverterIsSingleton() {

    Codec<Boolean, String> singleton = ConfigConverters.booleanConverter();

    for (int i = 0; i < 10; i++) {
      assertThat(ConfigConverters.booleanConverter() == singleton).isTrue();
    }
  }

  @Test
  public void byteConverterNotNull() {

    assertThat(ConfigConverters.byteConverter()).isNotNull();
  }

  @Test
  public void byteConverterIsSingleton() {

    Codec<Byte, String> singleton = ConfigConverters.byteConverter();

    for (int i = 0; i < 10; i++) {
      assertThat(ConfigConverters.byteConverter() == singleton).isTrue();
    }
  }

  @Test
  public void charConverterNotNull() {

    assertThat(ConfigConverters.charConverter()).isNotNull();
  }

  @Test
  public void charConverterIsSingleton() {

    Codec<Character, String> singleton = ConfigConverters.charConverter();

    for (int i = 0; i < 10; i++) {
      assertThat(ConfigConverters.charConverter() == singleton).isTrue();
    }
  }

  @Test
  public void shortConverterNotNull() {

    assertThat(ConfigConverters.shortConverter()).isNotNull();
  }

  @Test
  public void shortConverterIsSingleton() {

    Codec<Short, String> singleton = ConfigConverters.shortConverter();

    for (int i = 0; i < 10; i++) {
      assertThat(ConfigConverters.shortConverter() == singleton).isTrue();
    }
  }

  @Test
  public void intConverterNotNull() {

    assertThat(ConfigConverters.intConverter()).isNotNull();
  }

  @Test
  public void intConverterIsSingleton() {

    Codec<Integer, String> converter = ConfigConverters.intConverter();

    for (int i = 0; i < 10; i++) {
      assertThat(ConfigConverters.intConverter()).isEqualTo(converter);
    }
  }

  @Test
  public void longConverterNotNull() {

    assertThat(ConfigConverters.longConverter()).isNotNull();
  }

  @Test
  public void longConverterIsSingleton() {

    Codec<Long, String> singleton = ConfigConverters.longConverter();

    for (int i = 0; i < 10; i++) {

      assertThat(ConfigConverters.longConverter() == singleton).isTrue();
    }
  }

  @Test
  public void floatConverterNotNull() {

    assertThat(ConfigConverters.floatConverter()).isNotNull();
  }

  @Test
  public void floatConverterIsSingleton() {

    Codec<Float, String> singleton = ConfigConverters.floatConverter();

    for (int i = 0; i < 10; i++) {

      assertThat(ConfigConverters.floatConverter() == singleton).isTrue();
    }
  }

  @Test
  public void doubleConverterNotNull() {

    assertThat(ConfigConverters.doubleConverter()).isNotNull();
  }

  @Test
  public void doubleConverterIsSingleton() {

    Codec<Double, String> singleton = ConfigConverters.doubleConverter();

    for (int i = 0; i < 10; i++) {

      assertThat(ConfigConverters.doubleConverter() == singleton).isTrue();
    }
  }
}
