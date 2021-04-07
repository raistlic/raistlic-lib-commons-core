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

@SuppressWarnings("rawtypes")
enum GenericAssertionPassAll implements GenericAssertion {

  INSTANCE;

  @SuppressWarnings("unchecked")
  static <C> GenericAssertion<C> getInstance() {

    return (GenericAssertion<C>) INSTANCE;
  }

  @Override
  public Assertion isNull() {

    return this;
  }

  @Override
  public Assertion isNull(String message) {

    return this;
  }

  @Override
  public Assertion isNotNull() {

    return this;
  }

  @Override
  public Assertion isNotNull(String message) {

    return this;
  }

  @Override
  public Assertion isEqualTo(Object target) {

    return this;
  }

  @Override
  public Assertion isEqualTo(Object target, String message) {

    return this;
  }

  @Override
  public Assertion isNotEqualTo(Object target) {

    return this;
  }

  @Override
  public Assertion isNotEqualTo(Object target, String message) {

    return this;
  }

  @Override
  public Assertion matches(Predicate predicate) {

    return this;
  }

  @Override
  public Assertion matches(Predicate predicate, String message) {

    return this;
  }

  @Override
  public Assertion isInstanceOf(Class type, String message) {

    return this;
  }

  @Override
  public Assertion isInstanceOf(Class type) {

    return this;
  }
}
