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

package org.raistlic.common.adt;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.raistlic.common.precondition.InvalidParameterException;

import static org.fest.assertions.Assertions.assertThat;

/**
 * @author Lei CHEN (2015-03-03)
 */
public class BitMapTest {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @Test
  public void testSize() {

    for (int size : new int[]{1, 2, 3, 99, 102342}) {

      BitMap bitMap = BitMap.builder(size).build();
      assertThat(bitMap.size()).isEqualTo(size);
    }
  }

  @Test
  public void testRankOne() {

    String pattern = "01000 10101 11010 10110 01000 00101";
    BitMap bitMap = buildBitMap(pattern);

    assertThat(bitMap.rankOne(0)).isEqualTo(0);
    assertThat(bitMap.rankOne(1)).isEqualTo(1);
    assertThat(bitMap.rankOne(2)).isEqualTo(1);
    assertThat(bitMap.rankOne(3)).isEqualTo(1);
    assertThat(bitMap.rankOne(4)).isEqualTo(1);

    assertThat(bitMap.rankOne(5)).isEqualTo(2);
    assertThat(bitMap.rankOne(6)).isEqualTo(2);
    assertThat(bitMap.rankOne(7)).isEqualTo(3);
    assertThat(bitMap.rankOne(8)).isEqualTo(3);
    assertThat(bitMap.rankOne(9)).isEqualTo(4);

    assertThat(bitMap.rankOne(10)).isEqualTo(5);
    assertThat(bitMap.rankOne(11)).isEqualTo(6);
    assertThat(bitMap.rankOne(12)).isEqualTo(6);
    assertThat(bitMap.rankOne(13)).isEqualTo(7);
    assertThat(bitMap.rankOne(14)).isEqualTo(7);

    assertThat(bitMap.rankOne(15)).isEqualTo(8);
    assertThat(bitMap.rankOne(16)).isEqualTo(8);
    assertThat(bitMap.rankOne(17)).isEqualTo(9);
    assertThat(bitMap.rankOne(18)).isEqualTo(10);
    assertThat(bitMap.rankOne(19)).isEqualTo(10);

    assertThat(bitMap.rankOne(20)).isEqualTo(10);
    assertThat(bitMap.rankOne(21)).isEqualTo(11);
    assertThat(bitMap.rankOne(22)).isEqualTo(11);
    assertThat(bitMap.rankOne(23)).isEqualTo(11);
    assertThat(bitMap.rankOne(24)).isEqualTo(11);

    assertThat(bitMap.rankOne(25)).isEqualTo(11);
    assertThat(bitMap.rankOne(26)).isEqualTo(11);
    assertThat(bitMap.rankOne(27)).isEqualTo(12);
    assertThat(bitMap.rankOne(28)).isEqualTo(12);
    assertThat(bitMap.rankOne(29)).isEqualTo(13);
  }

  @Test
  public void testRankOneWithNegativeIndex() {

    String pattern = "01000 10101 11010 10110 01000 00101";
    BitMap bitMap = buildBitMap(pattern);

    exception.expect(InvalidParameterException.class);
    bitMap.rankOne(-1);
  }

  @Test
  public void testRankOneWithIndexGreaterThanSize() {

    String pattern = "01000 10101 11010 10110 01000 00101";
    BitMap bitMap = buildBitMap(pattern);

    exception.expect(InvalidParameterException.class);
    bitMap.rankOne(100);
  }

  @Test
  public void testRankOneWithIndexEqualToSize() {

    String pattern = "01000 10101 11010 10110 01000 00101";
    BitMap bitMap = buildBitMap(pattern);

    exception.expect(InvalidParameterException.class);
    bitMap.rankOne(pattern.replaceAll(" ", "").length());
  }

  @Test
  public void testRankZero() {

    String pattern = "01000 10101 11010 10110 01000 00101";
    BitMap bitMap = buildBitMap(pattern);

    assertThat(bitMap.rankZero(0)).isEqualTo(1);
    assertThat(bitMap.rankZero(1)).isEqualTo(1);
    assertThat(bitMap.rankZero(2)).isEqualTo(2);
    assertThat(bitMap.rankZero(3)).isEqualTo(3);
    assertThat(bitMap.rankZero(4)).isEqualTo(4);

    assertThat(bitMap.rankZero(5)).isEqualTo(4);
    assertThat(bitMap.rankZero(6)).isEqualTo(5);
    assertThat(bitMap.rankZero(7)).isEqualTo(5);
    assertThat(bitMap.rankZero(8)).isEqualTo(6);
    assertThat(bitMap.rankZero(9)).isEqualTo(6);

    assertThat(bitMap.rankZero(10)).isEqualTo(6);
    assertThat(bitMap.rankZero(11)).isEqualTo(6);
    assertThat(bitMap.rankZero(12)).isEqualTo(7);
    assertThat(bitMap.rankZero(13)).isEqualTo(7);
    assertThat(bitMap.rankZero(14)).isEqualTo(8);

    assertThat(bitMap.rankZero(15)).isEqualTo(8);
    assertThat(bitMap.rankZero(16)).isEqualTo(9);
    assertThat(bitMap.rankZero(17)).isEqualTo(9);
    assertThat(bitMap.rankZero(18)).isEqualTo(9);
    assertThat(bitMap.rankZero(19)).isEqualTo(10);

    assertThat(bitMap.rankZero(20)).isEqualTo(11);
    assertThat(bitMap.rankZero(21)).isEqualTo(11);
    assertThat(bitMap.rankZero(22)).isEqualTo(12);
    assertThat(bitMap.rankZero(23)).isEqualTo(13);
    assertThat(bitMap.rankZero(24)).isEqualTo(14);

    assertThat(bitMap.rankZero(25)).isEqualTo(15);
    assertThat(bitMap.rankZero(26)).isEqualTo(16);
    assertThat(bitMap.rankZero(27)).isEqualTo(16);
    assertThat(bitMap.rankZero(28)).isEqualTo(17);
    assertThat(bitMap.rankZero(29)).isEqualTo(17);
  }

  @Test
  public void testRankZeroWithNegativeIndex() {

    String pattern = "01000 10101 11010 10110 01000 00101";
    BitMap bitMap = buildBitMap(pattern);

    exception.expect(InvalidParameterException.class);
    assertThat(bitMap.rankZero(-1)).isPositive();
  }

  @Test
  public void testRankZeroWithIndexGreaterThanSize() {

    String pattern = "01000 10101 11010 10110 01000 00101";
    BitMap bitMap = buildBitMap(pattern);

    exception.expect(InvalidParameterException.class);
    assertThat(bitMap.rankZero(100)).isPositive();
  }

  @Test
  public void testRankZeroWithIndexEqualToSize() {

    String pattern = "01000 10101 11010 10110 01000 00101";
    BitMap bitMap = buildBitMap(pattern);

    exception.expect(InvalidParameterException.class);
    assertThat(bitMap.rankZero(pattern.replaceAll(" ", "").length())).isPositive();
  }

  @Test
  public void testSelectOne() {

    String pattern = "01000 10101 11010 10110 01000 00101";
    BitMap bitMap = buildBitMap(pattern);

    assertThat(bitMap.selectOne(0)).isEqualTo(1);
    assertThat(bitMap.selectOne(1)).isEqualTo(5);
    assertThat(bitMap.selectOne(2)).isEqualTo(7);
    assertThat(bitMap.selectOne(3)).isEqualTo(9);
    assertThat(bitMap.selectOne(4)).isEqualTo(10);
    assertThat(bitMap.selectOne(5)).isEqualTo(11);
    assertThat(bitMap.selectOne(6)).isEqualTo(13);
    assertThat(bitMap.selectOne(7)).isEqualTo(15);
    assertThat(bitMap.selectOne(8)).isEqualTo(17);
    assertThat(bitMap.selectOne(9)).isEqualTo(18);
    assertThat(bitMap.selectOne(10)).isEqualTo(21);
    assertThat(bitMap.selectOne(11)).isEqualTo(27);
    assertThat(bitMap.selectOne(12)).isEqualTo(29);
    assertThat(bitMap.selectOne(13)).isEqualTo(-1);
    assertThat(bitMap.selectOne(14)).isEqualTo(-1);
    assertThat(bitMap.selectOne(15)).isEqualTo(-1);
  }

  @Test
  public void testSelectOneWithNegativeIndex() {

    String pattern = "01000 10101 11010 10110 01000 00101";
    BitMap bitMap = buildBitMap(pattern);

    exception.expect(InvalidParameterException.class);
    assertThat(bitMap.selectOne(-1)).isEqualTo(-1);
  }

  @Test
  public void testSelectZero() {

    String pattern = "01000 10101 11010 10110 01000 00101";
    BitMap bitMap = buildBitMap(pattern);

    assertThat(bitMap.selectZero(0)).isEqualTo(0);
    assertThat(bitMap.selectZero(1)).isEqualTo(2);
    assertThat(bitMap.selectZero(2)).isEqualTo(3);
    assertThat(bitMap.selectZero(3)).isEqualTo(4);
    assertThat(bitMap.selectZero(4)).isEqualTo(6);
    assertThat(bitMap.selectZero(5)).isEqualTo(8);
    assertThat(bitMap.selectZero(6)).isEqualTo(12);
    assertThat(bitMap.selectZero(7)).isEqualTo(14);
    assertThat(bitMap.selectZero(8)).isEqualTo(16);
    assertThat(bitMap.selectZero(9)).isEqualTo(19);
    assertThat(bitMap.selectZero(10)).isEqualTo(20);
    assertThat(bitMap.selectZero(11)).isEqualTo(22);
    assertThat(bitMap.selectZero(12)).isEqualTo(23);
    assertThat(bitMap.selectZero(13)).isEqualTo(24);
    assertThat(bitMap.selectZero(14)).isEqualTo(25);
    assertThat(bitMap.selectZero(15)).isEqualTo(26);
    assertThat(bitMap.selectZero(16)).isEqualTo(28);
    assertThat(bitMap.selectZero(17)).isEqualTo(-1);
    assertThat(bitMap.selectZero(18)).isEqualTo(-1);
    assertThat(bitMap.selectZero(19)).isEqualTo(-1);
  }

  @Test
  public void testSelectZeroWithNegativeIndex() {

    String pattern = "01000 10101 11010 10110 01000 00101";
    BitMap bitMap = buildBitMap(pattern);

    exception.expect(InvalidParameterException.class);
    assertThat(bitMap.selectZero(-1)).isEqualTo(-1);
  }

  @Test
  public void testIsOne() {

    String pattern = "01000 10101 11010 10110 01000 00101";
    BitMap bitMap = buildBitMap(pattern);

    assertThat(bitMap.isOne(0)).isFalse();
    assertThat(bitMap.isOne(1)).isTrue();
    assertThat(bitMap.isOne(2)).isFalse();
    assertThat(bitMap.isOne(3)).isFalse();
    assertThat(bitMap.isOne(4)).isFalse();

    assertThat(bitMap.isOne(5)).isTrue();
    assertThat(bitMap.isOne(6)).isFalse();
    assertThat(bitMap.isOne(7)).isTrue();
    assertThat(bitMap.isOne(8)).isFalse();
    assertThat(bitMap.isOne(9)).isTrue();

    assertThat(bitMap.isOne(10)).isTrue();
    assertThat(bitMap.isOne(11)).isTrue();
    assertThat(bitMap.isOne(12)).isFalse();
    assertThat(bitMap.isOne(13)).isTrue();
    assertThat(bitMap.isOne(14)).isFalse();

    assertThat(bitMap.isOne(15)).isTrue();
    assertThat(bitMap.isOne(16)).isFalse();
    assertThat(bitMap.isOne(17)).isTrue();
    assertThat(bitMap.isOne(18)).isTrue();
    assertThat(bitMap.isOne(19)).isFalse();

    assertThat(bitMap.isOne(20)).isFalse();
    assertThat(bitMap.isOne(21)).isTrue();
    assertThat(bitMap.isOne(22)).isFalse();
    assertThat(bitMap.isOne(23)).isFalse();
    assertThat(bitMap.isOne(24)).isFalse();

    assertThat(bitMap.isOne(25)).isFalse();
    assertThat(bitMap.isOne(26)).isFalse();
    assertThat(bitMap.isOne(27)).isTrue();
    assertThat(bitMap.isOne(28)).isFalse();
    assertThat(bitMap.isOne(29)).isTrue();
  }

  @Test
  public void testIsOneWithNegativeIndex() {

    String pattern = "01000 10101 11010 10110 01000 00101";
    BitMap bitMap = buildBitMap(pattern);

    exception.expect(InvalidParameterException.class);
    assertThat(bitMap.isOne(-1)).isFalse();
  }

  @Test
  public void testIsOneWithIndexGreaterThanSize() {

    String pattern = "01000 10101 11010 10110 01000 00101";
    BitMap bitMap = buildBitMap(pattern);

    exception.expect(InvalidParameterException.class);
    assertThat(bitMap.isOne(100)).isFalse();
  }

  @Test
  public void testIsOneWithIndexEqualToSize() {

    String pattern = "01000 10101 11010 10110 01000 00101";
    BitMap bitMap = buildBitMap(pattern);

    exception.expect(InvalidParameterException.class);
    assertThat(bitMap.isOne(bitMap.size())).isFalse();
  }

  private static BitMap buildBitMap(String pattern) {

    pattern = pattern.replaceAll(" ", "");
    BitMap.Builder builder = BitMap.builder(pattern.length());
    for (int i = 0, len = pattern.length(); i < len; i++) {

      if (pattern.charAt(i) == '1') {
        builder.set(i);
      }
    }
    return builder.build();
  }
}
