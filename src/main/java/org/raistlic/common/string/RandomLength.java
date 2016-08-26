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

final class RandomLength {

  private int minimumLength;

  private int maximumLength;

  RandomLength() {

    this.minimumLength = DEFAULT_MIN_LENGTH;
    this.maximumLength = DEFAULT_MAX_LENGTH;
  }

  int getMinimumLength() {

    return minimumLength;
  }

  int getMaximumLength() {

    return maximumLength;
  }

  void withMinimumLength(int minimumLength) {

    Precondition.param(minimumLength).greaterThanOrEqualTo(0);

    this.minimumLength = minimumLength;
    this.maximumLength = Math.max(minimumLength, maximumLength);
  }

  void withMaximumLength(int maximumLength) {

    Precondition.param(maximumLength).greaterThanOrEqualTo(0);

    this.maximumLength = maximumLength;
    this.minimumLength = Math.min(maximumLength, minimumLength);
  }

  void withLength(int length) {

    Precondition.param(length).greaterThanOrEqualTo(0);

    this.minimumLength = length;
    this.maximumLength = length;
  }

  int getRandomLength() {

    return LocalUtil.randomLengthBetween(minimumLength, maximumLength);
  }

  private static final int DEFAULT_MIN_LENGTH = 3;

  private static final int DEFAULT_MAX_LENGTH = 8;
}
