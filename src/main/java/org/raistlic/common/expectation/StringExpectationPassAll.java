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

import java.util.function.Predicate;
import java.util.regex.Pattern;

enum StringExpectationPassAll implements StringExpectation {

  INSTANCE;

  @Override
  public StringExpectation isEmpty() {

    return this;
  }

  @Override
  public StringExpectation isEmpty(String message) {

    return this;
  }

  @Override
  public StringExpectation isNotEmpty() {

    return this;
  }

  @Override
  public StringExpectation isNotEmpty(String message) {

    return this;
  }

  @Override
  public StringExpectation isNullOrEmpty() {

    return this;
  }

  @Override
  public StringExpectation isNullOrEmpty(String message) {

    return this;
  }

  @Override
  public StringExpectation isNotNullOrEmpty() {

    return this;
  }

  @Override
  public StringExpectation isNotNullOrEmpty(String message) {

    return this;
  }

  @Override
  public StringExpectation hasLength(int length) {

    return this;
  }

  @Override
  public StringExpectation hasLength(int length, String message) {

    return this;
  }

  @Override
  public StringExpectation matchesPattern(Pattern pattern) {

    return this;
  }

  @Override
  public StringExpectation matchesPattern(Pattern pattern, String message) {

    return this;
  }

  @Override
  public StringExpectation matchesPattern(String pattern) {

    return this;
  }

  @Override
  public StringExpectation matchesPattern(String pattern, String message) {

    return this;
  }

  @Override
  public StringExpectation isNull() {

    return this;
  }

  @Override
  public StringExpectation isNull(String message) {

    return this;
  }

  @Override
  public StringExpectation isNotNull() {

    return this;
  }

  @Override
  public StringExpectation isNotNull(String message) {

    return this;
  }

  @Override
  public StringExpectation isEqualTo(String target) {

    return this;
  }

  @Override
  public StringExpectation isEqualTo(String target, String message) {

    return this;
  }

  @Override
  public StringExpectation isNotEqualTo(String target) {

    return this;
  }

  @Override
  public StringExpectation isNotEqualTo(String target, String message) {

    return this;
  }

  @Override
  public StringExpectation isInstanceOf(Class<?> type) {

    return this;
  }

  @Override
  public StringExpectation isInstanceOf(Class<?> type, String message) {

    return this;
  }

  @Override
  public StringExpectation matches(Predicate<? super String> predicate) {

    return this;
  }

  @Override
  public StringExpectation matches(Predicate<? super String> predicate, String message) {

    return this;
  }
}
