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

final class AssertionFactoryThreadLocal implements AssertionFactory {

  private final ThreadLocal<AssertionFactory> delegateFactoryHolder;

  private Function<String, ? extends RuntimeException> exceptionMapper;

  AssertionFactoryThreadLocal(Function<String, ? extends RuntimeException> exceptionMapper) {

    if (exceptionMapper == null) {
      throw new InvalidParameterException("'exceptionMapper' cannot be null.");
    }
    this.exceptionMapper = exceptionMapper;
    this.delegateFactoryHolder = new ThreadLocal<>();
  }

  @Override
  public BooleanAssertion expect(Boolean candidate) {

    return getLocalDelegateFactory().expect(candidate);
  }

  @Override
  public PrimitiveBooleanAssertion expect(boolean candidate) {

    return getLocalDelegateFactory().expect(candidate);
  }

  @Override
  public StringAssertion expect(String candidate) {

    return getLocalDelegateFactory().expect(candidate);
  }

  @Override
  public <V> GenericAssertion<V> expect(V candidate) {

    return getLocalDelegateFactory().expect(candidate);
  }

  @Override
  public <E> CollectionAssertion<E> expect(Collection<E> candidate) {

    return getLocalDelegateFactory().expect(candidate);
  }

  @Override
  public <N extends Number & Comparable<N>> NumberAssertion<N> expect(N candidate) {

    return getLocalDelegateFactory().expect(candidate);
  }

  @Override
  public ThreadAssertion expect(Thread thread) {

    return getLocalDelegateFactory().expect(thread);
  }

  @Override
  public void assertThat(boolean assertion, String message) {

    getLocalDelegateFactory().assertThat(assertion, message);
  }

  private AssertionFactory getLocalDelegateFactory() {

    AssertionFactory localFactory = delegateFactoryHolder.get();
    if (localFactory == null) {
      localFactory = new AssertionFactoryLazy(exceptionMapper);
      delegateFactoryHolder.set(localFactory);
    }
    return localFactory;
  }
}
