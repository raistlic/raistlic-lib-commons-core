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

import org.raistlic.common.Factory;

/**
 * @author Lei CHEN (2014-11-24)
 * @since 1.0
 */
public interface NumberTextForEnglishBuilder extends Factory<NumberText> {

  public static enum UpperCaseMode {

    UPPER_CASE_ALL,
    UPPER_CASE_FIRST_LETTER,
    LOWER_CASE,
    KEEP_ORIGINAL,
    ;
  }

  NumberTextForEnglishBuilder withMinusText(String minusText);

  NumberTextForEnglishBuilder withPointText(String pointText);

  NumberTextForEnglishBuilder withUpperCaseMode(UpperCaseMode mode);
}
