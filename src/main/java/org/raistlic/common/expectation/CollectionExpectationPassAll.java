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

import java.util.Collection;
import java.util.Comparator;
import java.util.function.Predicate;

@SuppressWarnings("rawtypes")
enum CollectionExpectationPassAll implements CollectionExpectation {

  INSTANCE;

  @SuppressWarnings("unchecked")
  static <C> CollectionExpectation<C> getInstance() {

    return INSTANCE;
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

  @Override
  public CollectionExpectation isEmpty() {

    return this;
  }

  @Override
  public CollectionExpectation isEmpty(String message) {

    return this;
  }

  @Override
  public CollectionExpectation isNullOrEmpty() {

    return this;
  }

  @Override
  public CollectionExpectation isNullOrEmpty(String message) {

    return this;
  }

  @Override
  public CollectionExpectation notEmpty() {

    return this;
  }

  @Override
  public CollectionExpectation notEmpty(String message) {

    return this;
  }

  @Override
  public CollectionExpectation hasSize(int size) {

    return this;
  }

  @Override
  public CollectionExpectation hasSize(int size, String message) {

    return this;
  }

  @Override
  public CollectionExpectation contains(Object element) {

    return this;
  }

  @Override
  public CollectionExpectation contains(Object element, String message) {

    return this;
  }

  @Override
  public CollectionExpectation containsAll(Collection elements) {

    return this;
  }

  @Override
  public CollectionExpectation containsAll(Collection elements, String message) {

    return this;
  }

  @Override
  public CollectionExpectation isOrderedBy(Comparator comparator, String message) {

    return this;
  }
}
