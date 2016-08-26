/*
 * Copyright 2016 Lei CHEN (raistlic@gmail.com)
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

package org.raistlic.common.expectation;

/**
 * Defines useful expectations on a {@link Boolean} candidate.
 */
public interface BooleanExpectation extends Expectation<Boolean, BooleanExpectation> {

  /**
   * Checks that the wrapped {@link Boolean} candidate is equal to {@link Boolean#TRUE} , or otherwise throws an
   * appropriate exception.
   */
  void isTrue();

  /**
   * Checks that the wrapped {@link Boolean} candidate is equal to {@link Boolean#TRUE} , or otherwise throws an
   * appropriate exception with the specified {@code message} .
   *
   * @param message the message used for creating exception when the check fails.
   */
  void isTrue(String message);

  /**
   * Checks that the wrapped {@link Boolean} candidate is equal to {@link Boolean#FALSE} , or otherwise throws an
   * appropriate exception.
   */
  void isFalse();

  /**
   * Checks that the wrapped {@link Boolean} candidate is equal to {@link Boolean#FALSE} , or otherwise throws an
   * appropriate exception with the specified {@code message} .
   *
   * @param message the message used for creating exception when the check fails.
   */
  void isFalse(String message);
}
