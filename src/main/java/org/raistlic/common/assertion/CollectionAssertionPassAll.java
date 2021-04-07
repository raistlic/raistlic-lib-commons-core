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

import java.util.Collection;
import java.util.Comparator;
import java.util.function.Predicate;

@SuppressWarnings("rawtypes")
enum CollectionAssertionPassAll implements CollectionAssertion {

  INSTANCE;

  @SuppressWarnings("unchecked")
  static <C> CollectionAssertion<C> getInstance() {

    return INSTANCE;
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

  @Override
  public CollectionAssertion isEmpty() {

    return this;
  }

  @Override
  public CollectionAssertion isEmpty(String message) {

    return this;
  }

  @Override
  public CollectionAssertion isNullOrEmpty() {

    return this;
  }

  @Override
  public CollectionAssertion isNullOrEmpty(String message) {

    return this;
  }

  @Override
  public CollectionAssertion notEmpty() {

    return this;
  }

  @Override
  public CollectionAssertion notEmpty(String message) {

    return this;
  }

  @Override
  public CollectionAssertion hasSize(int size) {

    return this;
  }

  @Override
  public CollectionAssertion hasSize(int size, String message) {

    return this;
  }

  @Override
  public CollectionAssertion contains(Object element) {

    return this;
  }

  @Override
  public CollectionAssertion contains(Object element, String message) {

    return this;
  }

  @Override
  public CollectionAssertion containsAll(Collection elements) {

    return this;
  }

  @Override
  public CollectionAssertion containsAll(Collection elements, String message) {

    return this;
  }

  @Override
  public CollectionAssertion isOrderedBy(Comparator comparator, String message) {

    return this;
  }
}
