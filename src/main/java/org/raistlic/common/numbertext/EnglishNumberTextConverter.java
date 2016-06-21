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
final class EnglishNumberTextConverter implements NumberTextConverter {

  private final EnglishLetterCase englishLetterCase;

  private final boolean dashConnectedTens;

  EnglishNumberTextConverter(boolean dashConnectedTens, EnglishLetterCase englishLetterCase) {

    assert englishLetterCase != null;

    this.dashConnectedTens = dashConnectedTens;
    this.englishLetterCase = englishLetterCase;
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
    String result = builder.toString();
    if (englishLetterCase == EnglishLetterCase.LOWER_CASE) {
      result = result.toLowerCase();
    }
    else if (englishLetterCase == EnglishLetterCase.UPPER_CASE) {
      result = result.toUpperCase();
    }
    return result;
  }

  private void buildText(StringBuilder builder, String number) {

    assert builder != null;

    if (number.startsWith("-")) {
      builder.append(getConnectDisplay(Connect.Minus))
              .append(getConnectDisplay(Connect.AfterMinus));
      number = number.substring(1);
    }

    int power = 0;
    while (number.length() > (power + 1) * 3)
      power++;

    while (power > 0) {
      boolean modified = extendToken(builder, number, power * 3);
      if (modified)
        builder.append(getConnectDisplay(Connect.AfterNumber))
                .append(getPowerDisplay(Power.values()[power - 1]));
      power--;
    }
    extendToken(builder, number, 0);
  }

  private boolean extendToken(StringBuilder builder,
                              String number,
                              int suffix) {

    assert builder != null && suffix < number.length();

    int len = number.length() - suffix;
    int hundreds = len > 2 ? (number.charAt(len - 3) - '0') : -1;
    int tens = len > 1 ? (number.charAt(len - 2) - '0') : -1;
    int inds = (number.charAt(len - 1) - '0');

    if (hundreds <= 0 && tens <= 0 && inds <= 0 && suffix > 0)
      return false;
    else if (len > 3)
      builder.append(getConnectDisplay(Connect.AfterPower));

    if (hundreds == 0) {
      if (len > 3 && (tens > 0 || inds > 0))
        builder.append(getConnectDisplay(Connect.And))
                .append(getConnectDisplay(Connect.AfterAnd));
    } else if (hundreds > 0) {
      builder.append(getDigitName(Digit.values()[hundreds]))
              .append(getConnectDisplay(Connect.AfterNumber))
              .append(getConnectDisplay(Connect.Hundred));
      if (tens > 0 || inds > 0)
        builder.append(getConnectDisplay(Connect.AfterHundred))
                .append(getConnectDisplay(Connect.And))
                .append(getConnectDisplay(Connect.AfterAnd));
    }

    if (tens > 1) {
      builder.append(getDigitMultiTen(Digit.values()[tens]));
      if (inds > 0)
        builder.append(getConnectDisplay(Connect.AfterTen));
    }

    if (tens == 1)
      builder.append(getDigitPlusTen(Digit.values()[inds]));
    else if (inds > 0 || number.length() == 1)
      builder.append(getDigitName(Digit.values()[inds]));

    return true;
  }

  private String getPowerDisplay(Power power) {

    return power.display;
  }

  private String getConnectDisplay(Connect connect) {

    if (connect == Connect.AfterTen) {
      return dashConnectedTens ? "-" : " ";
    }
    return connect.display;
  }

  private String getDigitName(Digit digit) {

    return digit.display;
  }

  private String getDigitPlusTen(Digit digit) {

    assert digit != null;

    return digit.plusTen;
  }

  private String getDigitMultiTen(Digit digit) {

    assert digit != null;

    return digit.multiTen;
  }

  private enum Connect {

    Minus        ("Minus"),
    Hundred      ("Hundred"),
    And          ("And"),
    AfterMinus   (" "),
    AfterNumber  (" "),
    AfterPower   (" "),
    AfterHundred (" "),
    AfterAnd     (" "),
    AfterTen     ("-"),;

    final String display;

    Connect(String display) {

      this.display = display;
    }
  }

  private enum Power {

    Thousand          ("Thousand"),          // 10 ^ 3
    Million           ("Million"),           // 10 ^ 6
    Billion           ("Billion"),           // 10 ^ 9
    Trillion          ("Trillion"),          // 10 ^ 12
    Quadrillion       ("Quadrillion"),       // 10 ^ 15
    Quintillion       ("Quintillion"),       // 10 ^ 18 (enough for Long.MAX_VALUE)
    Sextillion        ("Sextillion"),        // 10 ^ 21
    Septillion        ("Septillion"),        // 10 ^ 24
    Octillion         ("Octillion"),         // 10 ^ 27
    Nonillion         ("Nonillion"),         // 10 ^ 30
    Decillion         ("Decillion"),         // 10 ^ 33
    Undecillion       ("Undecillion"),       // 10 ^ 36
    Duodecillion      ("Duodecillion"),      // 10 ^ 39
    Tredecillion      ("Tredecillion"),      // 10 ^ 42
    Quattuordecillion ("Quattuordecillion"), // 10 ^ 45
    Quindecillion     ("Quindecillion"),     // 10 ^ 48
    Sexdecillion      ("Sexdecillion"),      // 10 ^ 51
    Septendecillion   ("Septendecillion"),   // 10 ^ 54
    Octodecillion     ("Octodecillion"),     // 10 ^ 57
    Novemdecillion    ("Novemdecillion"),    // 10 ^ 60
    Vigintillion      ("Vigintillion"),      // 10 ^ 63
    ;

    final String display;

    Power(String display) {

      this.display = display;
    }
  }

  private enum Digit {

    Zero  ("Zero", "Zeroth", "Ten", ""),
    One   ("One", "First", "Eleven", "Ten"),
    Two   ("Two", "Second", "Twelve", "Twenty"),
    Three ("Three", "Third", "Thirteen", "Thirty"),
    Four  ("Four", "Fourth", "Fourteen", "Fourty"),
    Five  ("Five", "Fifth", "Fifteen", "Fifty"),
    Six   ("Six", "Sixth", "Sixteen", "Sixty"),
    Seven ("Seven", "Seventh", "Seventeen", "Seventy"),
    Eight ("Eight", "Eighth", "Eighteen", "Eighty"),
    Nine  ("Nine", "Nineth", "Nineteen", "Ninety"),;

    final String display, displayOrdinal, plusTen, multiTen;

    Digit(String display, String displayOrdinal,
          String plusTen, String multiTen) {

      this.display = display;
      this.displayOrdinal = displayOrdinal;
      this.plusTen = plusTen;
      this.multiTen = multiTen;
    }
  }

  private static final int MAX_SUPPORTED_DIGITS = 63;

  private static final Predicate<String> NUMBER_RANGE_PREDICATE =
          LocalUtils.numberRangePredicate(MAX_SUPPORTED_DIGITS);
}
