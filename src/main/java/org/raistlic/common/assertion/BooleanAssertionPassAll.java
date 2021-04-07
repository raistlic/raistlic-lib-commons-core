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

enum BooleanAssertionPassAll implements BooleanAssertion {

  INSTANCE;

  @Override
  public void isTrue() {

  }

  @Override
  public void isTrue(String message) {

  }

  @Override
  public void isFalse() {

  }

  @Override
  public void isFalse(String message) {

  }

  @Override
  public BooleanAssertion isNull() {

    return this;
  }

  @Override
  public BooleanAssertion isNull(String message) {

    return this;
  }

  @Override
  public BooleanAssertion isNotNull() {

    return this;
  }

  @Override
  public BooleanAssertion isNotNull(String message) {

    return this;
  }

  @Override
  public BooleanAssertion isEqualTo(Boolean target) {

    return this;
  }

  @Override
  public BooleanAssertion isEqualTo(Boolean target, String message) {

    return this;
  }

  @Override
  public BooleanAssertion isNotEqualTo(Boolean target) {

    return this;
  }

  @Override
  public BooleanAssertion isNotEqualTo(Boolean target, String message) {

    return this;
  }

  @Override
  public BooleanAssertion isInstanceOf(Class<?> type) {

    return this;
  }

  @Override
  public BooleanAssertion isInstanceOf(Class<?> type, String message) {

    return this;
  }

  @Override
  public BooleanAssertion matches(Predicate<? super Boolean> predicate) {

    return this;
  }

  @Override
  public BooleanAssertion matches(Predicate<? super Boolean> predicate, String message) {

    return this;
  }
}
