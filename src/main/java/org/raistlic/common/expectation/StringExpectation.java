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

package org.raistlic.common.expectation;

import java.util.regex.Pattern;

public interface StringExpectation extends Expectation<String, StringExpectation> {

  StringExpectation isEmpty();

  StringExpectation isEmpty(String message);

  StringExpectation isNotEmpty();

  StringExpectation isNotEmpty(String message);

  StringExpectation isNullOrEmpty();

  StringExpectation isNullOrEmpty(String message);

  StringExpectation isNotNullOrEmpty();

  StringExpectation isNotNullOrEmpty(String message);

  StringExpectation hasLength(int length);

  StringExpectation hasLength(int length, String message);

  StringExpectation matchesPattern(Pattern pattern);

  StringExpectation matchesPattern(Pattern pattern, String message);

  StringExpectation matchesPattern(String pattern);

  StringExpectation matchesPattern(String pattern, String message);
}
