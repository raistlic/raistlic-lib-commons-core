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

/**
 * The interface of a converter, that's capable of converting a number into it's text description
 * in a natural language. An implementation of this interface may be dedicated for a specific
 * natural language, such as English.
 *
 * @author Lei CHEN (2014-11-24)
 * @since 1.0
 */
public interface NumberTextConverter {

  /**
   * The method converts the specified {@code number} into it's text description of a natural language.
   *
   * @param number the number to be converted, cannot be {@code null}.
   * @return the converted text description for the {@code number} .
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code number} is
   *         {@code null}, or when it's too big that exceeded the supported range of the implementation.
   */
  String convertToText(Number number) throws InvalidParameterException;

  /**
   * The method converts the specified {@code number} that's in the form of arabic numbers representation
   * into it's text description of a natural language.
   *
   * @param number the number to be converted, cannot be {@code null} or empty, and must be a valid
   *               arabic number representation.
   * @return the converted text description for the {@code number} .
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code number} is {@code null},
   *         invalid form, or too large.
   */
  String convertToText(String number) throws InvalidParameterException;
}
