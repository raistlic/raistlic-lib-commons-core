/*
 * Copyright 2016 Lei Chen (raistlic@gmail.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.raistlic.common.string;

import org.raistlic.common.precondition.Precondition;

import java.util.Random;

class RandomWordBuilder implements RandomStringBuilder {

  private String charSet;

  private RandomLength randomLength;

  RandomWordBuilder() {

    charSet = DEFAULT_CHAR_SET;
    randomLength = new RandomLength();
  }

  @Override
  public String build() {

    String chars = charSet;
    int length = randomLength.getRandomLength();
    StringBuilder builder = new StringBuilder();
    Random random = LocalUtil.getRandom();

    while (builder.length() < length) {
      char c = chars.charAt(random.nextInt(chars.length()));
      builder.append(c);
    }
    return builder.toString();
  }

  @Override
  public int getMinimumLength() {

    return randomLength.getMinimumLength();
  }

  @Override
  public int getMaximumLength() {

    return randomLength.getMaximumLength();
  }

  @Override
  public RandomStringBuilder withMinimumLength(int minimumLength) {

    randomLength.withMinimumLength(minimumLength);
    return this;
  }

  @Override
  public RandomStringBuilder withMaximumLength(int maximumLength) {

    randomLength.withMaximumLength(maximumLength);
    return this;
  }

  @Override
  public RandomStringBuilder withLength(int length) {

    randomLength.withLength(length);
    return this;
  }

  @Override
  public RandomStringBuilder withCharSet(String charSet) {

    Precondition.param(charSet)
      .isNotNull()
      .isNotEmpty();

    this.charSet = charSet;
    return this;
  }

  private static final String DEFAULT_CHAR_SET = createDefaultCharSet();

  private static String createDefaultCharSet() {

    StringBuilder builder = new StringBuilder();
    for (char c = 'A'; c <= 'Z'; c++) {
      builder.append(c).append(Character.toLowerCase(c));
    }
    return builder.toString();
  }
}
