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

@SuppressWarnings("rawtypes")
enum NumberExpectationPassAll implements NumberExpectation {

  INSTANCE;

  @SuppressWarnings("unchecked")
  static <C extends Number & Comparable<C>> NumberExpectation<C> getInstance() {

    return INSTANCE;
  }

  @Override
  public NumberExpectation greaterThan(Number target) {

    return this;
  }

  @Override
  public NumberExpectation greaterThan(Number target, String message) {

    return this;
  }

  @Override
  public NumberExpectation greaterThanOrEqualTo(Number target) {

    return this;
  }

  @Override
  public NumberExpectation greaterThanOrEqualTo(Number target, String message) {

    return this;
  }

  @Override
  public NumberExpectation lessThan(Number target) {

    return this;
  }

  @Override
  public NumberExpectation lessThan(Number target, String message) {

    return this;
  }

  @Override
  public NumberExpectation lessThanOrEqualTo(Number target) {

    return this;
  }

  @Override
  public NumberExpectation lessThanOrEqualTo(Number target, String message) {

    return this;
  }

  @Override
  public Expectation isNull() {

    return this;
  }

  @Override
  public Expectation isNull(String message) {

    return this;
  }

  @Override
  public Expectation isNotNull() {

    return this;
  }

  @Override
  public Expectation isNotNull(String message) {

    return this;
  }

  @Override
  public Expectation isEqualTo(Object target) {

    return this;
  }

  @Override
  public Expectation isEqualTo(Object target, String message) {

    return this;
  }

  @Override
  public Expectation isNotEqualTo(Object target) {

    return this;
  }

  @Override
  public Expectation isNotEqualTo(Object target, String message) {

    return this;
  }

  @Override
  public Expectation matches(Predicate predicate) {

    return this;
  }

  @Override
  public Expectation matches(Predicate predicate, String message) {

    return this;
  }

  @Override
  public Expectation isInstanceOf(Class type, String message) {

    return this;
  }

  @Override
  public Expectation isInstanceOf(Class type) {

    return this;
  }
}
