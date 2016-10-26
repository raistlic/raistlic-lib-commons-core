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

package org.raistlic.common.assertion;

import java.util.regex.Pattern;

public interface StringAssertion extends Assertion<String, StringAssertion> {

  StringAssertion isEmpty();

  StringAssertion isEmpty(String message);

  StringAssertion isNotEmpty();

  StringAssertion isNotEmpty(String message);

  StringAssertion isNullOrEmpty();

  StringAssertion isNullOrEmpty(String message);

  StringAssertion isNotNullOrEmpty();

  StringAssertion isNotNullOrEmpty(String message);

  StringAssertion hasLength(int length);

  StringAssertion hasLength(int length, String message);

  StringAssertion matchesPattern(Pattern pattern);

  StringAssertion matchesPattern(Pattern pattern, String message);

  StringAssertion matchesPattern(String pattern);

  StringAssertion matchesPattern(String pattern, String message);
}
