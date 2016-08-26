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

/**
 * The default implementation for {@link ExpectedCases} .
 *
 * @author Lei Chen (2015-12-29)
 */
final class ExpectedCasesDefault implements ExpectedCases {

  private final Function<String, ? extends RuntimeException> exceptionMapper;

  ExpectedCasesDefault(Function<String, ? extends RuntimeException> exceptionMapper) {

    if (exceptionMapper == null) {
      throw new InvalidParameterException("'exceptionMapper' cannot be null.");
    }
    this.exceptionMapper = exceptionMapper;
  }

  @Override
  public BooleanExpectation expect(Boolean value) {

    return new BooleanExpectationDefault(value, exceptionMapper);
  }

  @Override
  public PrimitiveBooleanExpectation expect(boolean value) {

    return new PrimitiveBooleanExpectationDefault(value, exceptionMapper);
  }

  @Override
  public StringExpectation expect(String value) {

    return new StringExpectationDefault(value, exceptionMapper);
  }

  @Override
  public <E> GenericExpectation<E> expect(E value) {

    return new GenericExpectationDefault<>(value, exceptionMapper);
  }

  @Override
  public <E> CollectionExpectation<E> expect(Collection<E> collection) {

    return new CollectionExpectationDefault<>(collection, exceptionMapper);
  }

  @Override
  public <N extends Number & Comparable<N>> NumberExpectation<N> expect(N value) {

    return new NumberExpectationDefault<>(value, exceptionMapper);
  }

  @Override
  public ThreadExpectation expect(Thread thread) {

    return new ThreadExpectationDefault(thread, exceptionMapper);
  }

  @Override
  public void assertThat(boolean assertion, String message) {

    if (!assertion) {
      throw exceptionMapper.apply(message);
    }
  }
}
