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

import java.util.function.Predicate;
import java.util.regex.Pattern;

/**
 * Dummy singleton implementation for {@link StringAssertion} that blindly passes all checks.
 */
enum StringAssertionPassAll implements StringAssertion {

  INSTANCE;

  @Override
  public StringAssertion isEmpty() {

    return this;
  }

  @Override
  public StringAssertion isEmpty(String message) {

    return this;
  }

  @Override
  public StringAssertion isNotEmpty() {

    return this;
  }

  @Override
  public StringAssertion isNotEmpty(String message) {

    return this;
  }

  @Override
  public StringAssertion isNullOrEmpty() {

    return this;
  }

  @Override
  public StringAssertion isNullOrEmpty(String message) {

    return this;
  }

  @Override
  public StringAssertion isNotNullOrEmpty() {

    return this;
  }

  @Override
  public StringAssertion isNotNullOrEmpty(String message) {

    return this;
  }

  @Override
  public StringAssertion hasLength(int length) {

    return this;
  }

  @Override
  public StringAssertion hasLength(int length, String message) {

    return this;
  }

  @Override
  public StringAssertion matchesPattern(Pattern pattern) {

    return this;
  }

  @Override
  public StringAssertion matchesPattern(Pattern pattern, String message) {

    return this;
  }

  @Override
  public StringAssertion matchesPattern(String pattern) {

    return this;
  }

  @Override
  public StringAssertion matchesPattern(String pattern, String message) {

    return this;
  }

  @Override
  public StringAssertion isNull() {

    return this;
  }

  @Override
  public StringAssertion isNull(String message) {

    return this;
  }

  @Override
  public StringAssertion isNotNull() {

    return this;
  }

  @Override
  public StringAssertion isNotNull(String message) {

    return this;
  }

  @Override
  public StringAssertion isEqualTo(String target) {

    return this;
  }

  @Override
  public StringAssertion isEqualTo(String target, String message) {

    return this;
  }

  @Override
  public StringAssertion isNotEqualTo(String target) {

    return this;
  }

  @Override
  public StringAssertion isNotEqualTo(String target, String message) {

    return this;
  }

  @Override
  public StringAssertion isInstanceOf(Class<?> type) {

    return this;
  }

  @Override
  public StringAssertion isInstanceOf(Class<?> type, String message) {

    return this;
  }

  @Override
  public StringAssertion matches(Predicate<? super String> predicate) {

    return this;
  }

  @Override
  public StringAssertion matches(Predicate<? super String> predicate, String message) {

    return this;
  }
}
