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

import org.raistlic.common.precondition.InvalidParameterException;
import org.raistlic.common.precondition.Precondition;

import java.util.Collection;
import java.util.concurrent.atomic.AtomicBoolean;

final class ExpectedCasesSwitchableProxy implements ExpectedCases {

  private final ExpectedCases original;

  private final AtomicBoolean switchFlag;

  ExpectedCasesSwitchableProxy(ExpectedCases original, AtomicBoolean switchFlag) {

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
  public BooleanExpectation expect(Boolean candidate) {

    if (switchFlag.get()) {
      return original.expect(candidate);
    }
    else {
      return BooleanExpectationPassAll.INSTANCE;
    }
  }

  @Override
  public PrimitiveBooleanExpectation expect(boolean candidate) {

    if (switchFlag.get()) {
      return original.expect(candidate);
    }
    else {
      return PrimitiveBooleanExpectationPassAll.INSTANCE;
    }
  }

  @Override
  public StringExpectation expect(String candidate) {

    if (switchFlag.get()) {
      return original.expect(candidate);
    }
    else {
      return StringExpectationPassAll.INSTANCE;
    }
  }

  @Override
  public <V> GenericExpectation<V> expect(V candidate) {

    if (switchFlag.get()) {
      return original.expect(candidate);
    }
    else {
      return GenericExpectationPassAll.getInstance();
    }
  }

  @Override
  public <E> CollectionExpectation<E> expect(Collection<E> candidate) {

    if (switchFlag.get()) {
      return original.expect(candidate);
    }
    else {
      return CollectionExpectationPassAll.getInstance();
    }
  }

  @Override
  public <N extends Number & Comparable<N>> NumberExpectation<N> expect(N candidate) {

    if (switchFlag.get()) {
      return original.expect(candidate);
    }
    else {
      return NumberExpectationPassAll.getInstance();
    }
  }

  @Override
  public ThreadExpectation expect(Thread thread) {

    if (switchFlag.get()) {
      return original.expect(thread);
    }
    else {
      return ThreadExpectationPassAll.INSTANCE;
    }
  }

  @Override
  public void assertThat(boolean assertion, String message) {

    if (switchFlag.get()) {
      original.assertThat(assertion, message);
    }
  }
}
