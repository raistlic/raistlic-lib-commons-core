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
import java.util.function.Function;

/**
 * The default implementation for {@link AssertionFactory} .
 *
 * @author Lei Chen (2015-12-29)
 */
final class AssertionFactoryDefault implements AssertionFactory {

  private final Function<String, ? extends RuntimeException> exceptionMapper;

  AssertionFactoryDefault(Function<String, ? extends RuntimeException> exceptionMapper) {

    if (exceptionMapper == null) {
      throw new InvalidParameterException("'exceptionMapper' cannot be null.");
    }
    this.exceptionMapper = exceptionMapper;
  }

  @Override
  public BooleanAssertion expect(Boolean value) {

    return new BooleanAssertionDefault(value, exceptionMapper);
  }

  @Override
  public PrimitiveBooleanAssertion expect(boolean value) {

    return new PrimitiveBooleanAssertionDefault(value, exceptionMapper);
  }

  @Override
  public StringAssertion expect(String value) {

    return new StringAssertionDefault(value, exceptionMapper);
  }

  @Override
  public <E> GenericAssertion<E> expect(E value) {

    return new GenericAssertionDefault<>(value, exceptionMapper);
  }

  @Override
  public <E> CollectionAssertion<E> expect(Collection<E> collection) {

    return new CollectionAssertionDefault<>(collection, exceptionMapper);
  }

  @Override
  public <N extends Number & Comparable<N>> NumberAssertion<N> expect(N value) {

    return new NumberAssertionDefault<>(value, exceptionMapper);
  }

  @Override
  public ThreadAssertion expect(Thread thread) {

    return new ThreadAssertionDefault(thread, exceptionMapper);
  }

  @Override
  public void assertThat(boolean assertion, String message) {

    if (!assertion) {
      throw exceptionMapper.apply(message);
    }
  }
}
