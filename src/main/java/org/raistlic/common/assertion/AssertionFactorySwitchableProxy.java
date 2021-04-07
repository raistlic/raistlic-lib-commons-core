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

import org.raistlic.common.precondition.InvalidParameterException;

import java.util.Collection;
import java.util.concurrent.atomic.AtomicBoolean;

final class AssertionFactorySwitchableProxy implements AssertionFactory {

  private final AssertionFactory original;

  private final AtomicBoolean switchFlag;

  AssertionFactorySwitchableProxy(AssertionFactory original, AtomicBoolean switchFlag) {

    if (original == null) {
      throw new InvalidParameterException();
    }
    if (switchFlag == null) {
      throw new InvalidParameterException();
    }
    this.original = original;
    this.switchFlag = switchFlag;
  }

  @Override
  public BooleanAssertion expect(Boolean candidate) {

    if (switchFlag.get()) {
      return original.expect(candidate);
    } else {
      return BooleanAssertionPassAll.INSTANCE;
    }
  }

  @Override
  public PrimitiveBooleanAssertion expect(boolean candidate) {

    if (switchFlag.get()) {
      return original.expect(candidate);
    } else {
      return PrimitiveBooleanAssertionPassAll.INSTANCE;
    }
  }

  @Override
  public StringAssertion expect(String candidate) {

    if (switchFlag.get()) {
      return original.expect(candidate);
    } else {
      return StringAssertionPassAll.INSTANCE;
    }
  }

  @Override
  public <V> GenericAssertion<V> expect(V candidate) {

    if (switchFlag.get()) {
      return original.expect(candidate);
    } else {
      return GenericAssertionPassAll.getInstance();
    }
  }

  @Override
  public <E> CollectionAssertion<E> expect(Collection<E> candidate) {

    if (switchFlag.get()) {
      return original.expect(candidate);
    } else {
      return CollectionAssertionPassAll.getInstance();
    }
  }

  @Override
  public <N extends Number & Comparable<N>> NumberAssertion<N> expect(N candidate) {

    if (switchFlag.get()) {
      return original.expect(candidate);
    } else {
      return NumberAssertionPassAll.getInstance();
    }
  }

  @Override
  public ThreadAssertion expect(Thread thread) {

    if (switchFlag.get()) {
      return original.expect(thread);
    } else {
      return ThreadAssertionPassAll.INSTANCE;
    }
  }

  @Override
  public void assertThat(boolean assertion, String message) {

    if (switchFlag.get()) {
      original.assertThat(assertion, message);
    }
  }
}
