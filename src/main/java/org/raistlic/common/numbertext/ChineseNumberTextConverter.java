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

package org.raistlic.common.numbertext;

import org.raistlic.common.precondition.InvalidParameterException;
import org.raistlic.common.precondition.Precondition;

import java.util.function.Predicate;

/**
 * @author Lei CHEN (2015-11-19)
 */
final class ChineseNumberTextConverter implements NumberTextConverter {

  static final ChineseNumberTextConverter SIMPLIFIED = new ChineseNumberTextConverter(Type.SIMPLIFIED);

  static final ChineseNumberTextConverter TRADITIONAL = new ChineseNumberTextConverter(Type.TRADITIONAL);

  private final Type type;

  private ChineseNumberTextConverter(Type type) {

    assert type != null;

    this.type = type;
  }

  @Override
  public String convertToText(Number number) throws InvalidParameterException {

    Precondition.param(number).isNotNull();

    return convertToText(number.toString());
  }

  @Override
  public String convertToText(String number) throws InvalidParameterException {

    Precondition.param(number).isNotNull();
    Precondition.param(number).matchesPattern(LocalUtils.VALID_NUMBER_PATTERN);
    Precondition.param(number).matches(NUMBER_RANGE_PREDICATE);

    StringBuilder builder = new StringBuilder();
    buildText(builder, number);
    return builder.toString();
  }

  private void buildText(StringBuilder builder, String number) {

    assert builder != null;

    if (number.startsWith("-")) {
      builder.append(getConnectDisplay(Connect.Fu));
      number = number.substring(1);
    }

    int power = 0;
    while (number.length() > (power + 1) * 4)
      power++;

    while (power > 0) {
      if (extendToken(builder, number, power * 4))
        builder.append(getPowerDisplay(Power.values()[power - 1]));
      power--;
    }
    extendToken(builder, number, 0);
  }

  private boolean extendToken(StringBuilder builder,
                              String number,
                              int suffix) {

    assert builder != null && number.length() > suffix;

    int len = number.length() - suffix;
    int qian = len > 3 ? (number.charAt(len - 4) - '0') : -1;
    int bai = len > 2 ? (number.charAt(len - 3) - '0') : -1;
    int shi = len > 1 ? (number.charAt(len - 2) - '0') : -1;
    int ind = (number.charAt(len - 1) - '0');

    boolean nonZero = false; // true if any of the digits is not zero
    if (qian == 0) {
      if (bai > 0 || shi > 0 || ind > 0)
        builder.append(getConnectDisplay(Connect.Ling));
    } else if (qian > 0) {
      builder.append(getDigitDisplay(Digit.values()[qian]))
        .append(getConnectDisplay(Connect.Qian));
      nonZero = true;
    }

    if (bai == 0) {
      if (qian > 0 && (shi > 0 || ind > 0))
        builder.append(getConnectDisplay(Connect.Ling));
    } else if (bai > 0) {
      builder.append(getDigitDisplay(Digit.values()[bai]))
        .append(getConnectDisplay(Connect.Bai));
      nonZero = true;
    }

    if (shi == 0) {
      if (bai > 0 && ind > 0)
        builder.append(getConnectDisplay(Connect.Ling));
    } else if (shi > 0) {
      if (number.length() > 2 || shi != 1)
        builder.append(getDigitDisplay(Digit.values()[shi]));
      builder.append(getConnectDisplay(Connect.Shi));
      nonZero = true;
    }

    if (ind == 0) {
      boolean addZero = len == 1;
      for (int i = 1; addZero && i <= suffix; i++) {
        if (number.charAt(i) != '0')
          addZero = false;
      }
      if (addZero) builder.append(getConnectDisplay(Connect.Ling));
    } else {
      builder.append(getDigitDisplay(Digit.values()[ind]));
      nonZero = true;
    }
    return nonZero;
  }

  private String getConnectDisplay(Connect connect) {

    assert connect != null;

    return type == Type.SIMPLIFIED ?
      connect.display :
      connect.displayTraditional;
  }

  private String getPowerDisplay(Power power) {

    assert power != null;

    return type == Type.SIMPLIFIED ?
      power.display :
      power.displayTraditional;
  }

  private String getDigitDisplay(Digit digit) {

    assert digit != null;

    return type == Type.SIMPLIFIED ?
      digit.display :
      digit.displayTraditional;
  }

  private enum Type {

    SIMPLIFIED, TRADITIONAL
  }

  private enum Connect {

    Di("第", "第"),
    Fu("负", "負"),
    Ling("零", "零"),
    Shi("十", "拾"),
    Bai("百", "佰"),
    Qian("千", "仟"),
    ;

    final String display, displayTraditional;

    Connect(String display, String displayTraditional) {

      this.display = display;
      this.displayTraditional = displayTraditional;
    }
  }

  private enum Power {

    Wan("万", "萬"), // 10^4
    Yi("亿", "億"), // 10^8
    Zhao("兆", "兆"), // 10^12
    Jing("京", "京"), // 10^16 (enough for Long.MAX_VALUE)
    Gai("垓", "垓"), // 10^20
    Zi("秭", "秭"), // 10^24
    Rang("穰", "穰"), // 10^28
    Gou("沟", "溝"), // 10^32
    Jian("涧", "澗"), // 10^36
    Zheng("正", "正"), // 10^40
    Zai("载", "載"), // 10^44
    ;

    final String display, displayTraditional;

    Power(String display, String displayTraditional) {

      this.display = display;
      this.displayTraditional = displayTraditional;
    }
  }

  private enum Digit {

    Ling("零", "零"), // just to occupy this position
    Yi("一", "壹"),
    Er("二", "贰"),
    San("三", "叁"),
    Si("四", "肆"),
    Wu("五", "伍"),
    Liu("六", "陆"),
    Qi("七", "柒"),
    Ba("八", "捌"),
    Jiu("九", "玖"),
    ;

    final String display, displayTraditional;

    Digit(String display, String displayTraditional) {

      this.display = display;
      this.displayTraditional = displayTraditional;
    }
  }

  private static final int MAX_SUPPORTED_DIGITS = 44;

  private static final Predicate<String> NUMBER_RANGE_PREDICATE =
    LocalUtils.numberRangePredicate(MAX_SUPPORTED_DIGITS);
}
