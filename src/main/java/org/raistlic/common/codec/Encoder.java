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

package org.raistlic.common.codec;

/**
 * This instance defines the parser that parses the referenced type {@code S} to
 * the referenced type {@code D}.
 *
 * @author Lei CHEN
 * @since 1.0
 */
public interface Encoder<S, D> {

  /**
   * The method checks whether the specified {@code src} is in valid form for encoding.
   *
   * @param src the value to be checked.
   * @return {@code true} if {@code src} is a valid value to be encoded.
   * @throws java.lang.UnsupportedOperationException if the implementation does not support this
   *         upfront check.
   * @deprecated to be deprecated in the next release(1.5), now throws UnsupportedOperationException
   *             by default, this is to be friendly to lambda expression.
   */
  @Deprecated
  default boolean isValidSrc(S src) {

    throw new UnsupportedOperationException();
  }

  /**
   * The method encodes the specified {@code src} , into type {@code D} .
   *
   * @param src the value to be encoded.
   * @return the encode result.
   * @throws org.raistlic.common.codec.ValueConversionException if anything goes wrong in the
   *         process of encoding.
   */
  D encode(S src) throws ValueConversionException;
}
