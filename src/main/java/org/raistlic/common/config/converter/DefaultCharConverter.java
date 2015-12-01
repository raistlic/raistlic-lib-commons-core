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

package org.raistlic.common.config.converter;

import org.raistlic.common.codec.Codec;
import org.raistlic.common.codec.ValueConversionException;
import org.raistlic.common.config.exception.ConfigValueConvertException;

/**
 * @author Lei Chen (2015-09-14)
 */
enum DefaultCharConverter implements Codec<Character, String> {

  INSTANCE;

  @Override
  public boolean isValidDest(String dest) {

    return dest == null || dest.length() == 1;
  }

  @Override
  public Character decode(String dest) throws ValueConversionException {

    if (dest == null) {
      return null;
    }
    else if (dest.length() == 1) {
      return dest.charAt(0);
    }
    else {
      throw new ConfigValueConvertException("Cannot convert value to char: '" + dest + "'");
    }
  }

  @Override
  public boolean isValidSrc(Character src) {

    return true;
  }

  @Override
  public String encode(Character src) throws ValueConversionException {

    return (src == null) ? null : src.toString();
  }
}
