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

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * @author Lei CHEN (2014-11-24)
 * @since 1.0
 */
public interface NumberText {

  String toText(long number);

  String toText(BigInteger number) throws IllegalArgumentException;

  String toText(BigDecimal number) throws IllegalArgumentException;

  String toText(String number) throws IllegalArgumentException;
}
