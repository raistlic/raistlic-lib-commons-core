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

import org.raistlic.common.precondition.InvalidParameterException;

/**
 * This instance defines the parser that parses the referenced type {@code D} to
 * the referenced type {@code S}.
 *
 * @author Lei CHEN
 * @since 1.0
 */
public interface Decoder<S, T> {

  /**
   * The method decodes the specified {@code dest} of type {@code D} , into type {@code S} .
   * 
   * @param target the dest to be decoded.
   * @return the decode result.
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code target} is
   *         {@code null}.
   * @throws org.raistlic.common.codec.ValueConversionException if anything goes wrong in the
   *         process of decoding the value.
   */
  S decode(T target) throws InvalidParameterException, ValueConversionException;
}
