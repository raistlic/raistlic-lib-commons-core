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
enum DefaultBooleanConverter implements Codec<Boolean, String> {

  INSTANCE;

  private static final String TRUE = "TRUE";

  private static final String FALSE = "FALSE";

  @Override
  public Boolean decode(String target) throws ValueConversionException {

    if (target == null) {
      return null;
    }
    String trimmed = target.trim().toUpperCase();
    if (trimmed.equals(TRUE)) {
      return true;
    }
    else if (trimmed.equals(FALSE)) {
      return false;
    }
    else {
      throw new ConfigValueConvertException("Cannot convert value to boolean: '" + target + "'");
    }
  }

  @Override
  public String encode(Boolean src) throws ValueConversionException {

    return src == null ? null : src.toString();
  }
}
