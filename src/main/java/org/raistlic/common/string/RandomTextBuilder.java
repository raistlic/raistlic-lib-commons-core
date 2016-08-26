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

class RandomTextBuilder implements RandomStringBuilder {

  private RandomLength randomLength;

  private RandomLength wordRandomLength;

  private RandomStringBuilder wordBuilder;

  private String delimiter;

  RandomTextBuilder(RandomStringBuilder wordBuilder) {

    this.randomLength = new RandomLength();
    this.wordBuilder = wordBuilder;
    this.delimiter = DEFAULT_DELIMITER;
    this.wordRandomLength = new RandomLength();
    this.wordRandomLength.withMinimumLength(wordBuilder.getMinimumLength());
    this.wordRandomLength.withMaximumLength(wordBuilder.getMaximumLength());
  }

  @Override
  public String build() {

    int length = randomLength.getRandomLength();

    StringBuilder builder = new StringBuilder();
    for (int wordLength = wordRandomLength.getRandomLength();
         length > 0;
         wordLength = wordRandomLength.getRandomLength()) {

      String delim = builder.length() > 0 ? this.delimiter : "";
      wordLength = Math.min(length - delim.length(), wordLength);
      length -= wordLength + delim.length();
      if (length <= delim.length()) {
        wordLength += length;
        length = 0;
      }

      String word = wordBuilder.withLength(wordLength).build();
      builder.append(delim).append(word);
    }

    wordBuilder.withMinimumLength(wordRandomLength.getMinimumLength())
        .withMaximumLength(wordRandomLength.getMaximumLength());
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
  public RandomStringBuilder withCharSet(String charset) {

    throw new UnsupportedOperationException();
  }

  private static final String DEFAULT_DELIMITER = " ";
}
