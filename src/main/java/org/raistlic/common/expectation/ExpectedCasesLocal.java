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

import java.util.Collection;
import java.util.function.Function;

final class ExpectedCasesLocal implements ExpectedCases {

  private final ThreadLocal<BooleanExpectationDefault> booleanExpectationLocal = new ThreadLocal<>();

  private final ThreadLocal<PrimitiveBooleanExpectationDefault> primitiveBooleanExpectationLocal = new ThreadLocal<>();

  private Function<String, ? extends RuntimeException> exceptionMapper;

  ExpectedCasesLocal(Function<String, ? extends RuntimeException> exceptionMapper) {

    if (exceptionMapper == null) {
      throw new InvalidParameterException("'exceptionMapper' cannot be null.");
    }
    this.exceptionMapper = exceptionMapper;
  }

  @Override
  public BooleanExpectation expect(Boolean candidate) {

    BooleanExpectationDefault booleanExpectation = booleanExpectationLocal.get();
    if (booleanExpectation == null) {
      booleanExpectation = new BooleanExpectationDefault(candidate, exceptionMapper);
      booleanExpectationLocal.set(booleanExpectation);
    }
    else {
      booleanExpectation.setCandidate(candidate);
    }
    return booleanExpectation;
  }

  @Override
  public PrimitiveBooleanExpectation expect(boolean candidate) {

    PrimitiveBooleanExpectationDefault primitiveBooleanExpectation = primitiveBooleanExpectationLocal.get();
    if (primitiveBooleanExpectation == null) {
      primitiveBooleanExpectation = new PrimitiveBooleanExpectationDefault(candidate, exceptionMapper);
      primitiveBooleanExpectationLocal.set(primitiveBooleanExpectation);
    }
    else {
      primitiveBooleanExpectation.setCandidate(candidate);
    }
    return primitiveBooleanExpectation;
  }

  @Override
  public StringExpectation expect(String candidate) {

    return null;
  }

  @Override
  public <V> GenericExpectation<V> expect(V candidate) {

    return null;
  }

  @Override
  public <E> CollectionExpectation<E> expect(Collection<E> candidate) {

    return null;
  }

  @Override
  public <N extends Number & Comparable<N>> NumberExpectation<N> expect(N candidate) {

    return null;
  }

  @Override
  public ThreadExpectation expect(Thread thread) {

    return null;
  }

  @Override
  public void assertThat(boolean assertion, String message) {

  }

  void clear() {

    booleanExpectationLocal.set(null);
    primitiveBooleanExpectationLocal.set(null);
  }
}
