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

import java.security.InvalidParameterException;
import java.util.Collection;
import java.util.function.Function;

/**
 * Lazily instantiate different types of assertions, and reuse them. This class works only in a
 * single threaded context.
 */
final class AssertionFactoryLazy implements AssertionFactory {

  private final Function<String, ? extends RuntimeException> exceptionMapper;

  private BooleanAssertionDefault booleanAssertionDefault;

  private PrimitiveBooleanAssertionDefault primitiveBooleanAssertionDefault;

  private StringAssertionDefault stringAssertionDefault;

  @SuppressWarnings("rawtypes")
  private GenericAssertionDefault genericAssertionDefault;

  @SuppressWarnings("rawtypes")
  private CollectionAssertionDefault collectionAssertionDefault;

  @SuppressWarnings("rawtypes")
  private NumberAssertionDefault numberAssertionDefault;

  private ThreadAssertionDefault threadAssertionDefault;

  AssertionFactoryLazy(Function<String, ? extends RuntimeException> exceptionMapper) {

    if (exceptionMapper == null) {
      throw new InvalidParameterException("'exceptionMapper' cannot be null.");
    }
    this.exceptionMapper = exceptionMapper;
  }

  @Override
  public BooleanAssertion expect(Boolean candidate) {

    if (booleanAssertionDefault == null) {
      booleanAssertionDefault = new BooleanAssertionDefault(candidate, exceptionMapper);
    } else {
      booleanAssertionDefault.setCandidate(candidate);
    }
    return booleanAssertionDefault;
  }

  @Override
  public PrimitiveBooleanAssertion expect(boolean candidate) {

    if (primitiveBooleanAssertionDefault == null) {
      primitiveBooleanAssertionDefault = new PrimitiveBooleanAssertionDefault(candidate, exceptionMapper);
    } else {
      primitiveBooleanAssertionDefault.setCandidate(candidate);
    }
    return primitiveBooleanAssertionDefault;
  }

  @Override
  public StringAssertion expect(String candidate) {

    if (stringAssertionDefault == null) {
      stringAssertionDefault = new StringAssertionDefault(candidate, exceptionMapper);
    } else {
      stringAssertionDefault.setCandidate(candidate);
    }
    return stringAssertionDefault;
  }

  @Override
  @SuppressWarnings({"unchecked", "rawtypes"})
  public <V> GenericAssertion<V> expect(V candidate) {

    if (genericAssertionDefault == null) {
      genericAssertionDefault = new GenericAssertionDefault(candidate, exceptionMapper);
    } else {
      genericAssertionDefault.setCandidate(candidate);
    }
    return (GenericAssertion<V>) genericAssertionDefault;
  }

  @Override
  @SuppressWarnings({"unchecked", "rawtypes"})
  public <E> CollectionAssertion<E> expect(Collection<E> candidate) {

    if (collectionAssertionDefault == null) {
      collectionAssertionDefault = new CollectionAssertionDefault(candidate, exceptionMapper);
    } else {
      collectionAssertionDefault.setCandidate(candidate);
    }
    return (CollectionAssertion<E>) collectionAssertionDefault;
  }

  @Override
  @SuppressWarnings({"unchecked", "rawtypes"})
  public <N extends Number & Comparable<N>> NumberAssertion<N> expect(N candidate) {

    if (numberAssertionDefault == null) {
      numberAssertionDefault = new NumberAssertionDefault(candidate, exceptionMapper);
    } else {
      numberAssertionDefault.setCandidate(candidate);
    }
    return (NumberAssertion<N>) numberAssertionDefault;
  }

  @Override
  public ThreadAssertion expect(Thread thread) {

    if (threadAssertionDefault == null) {
      threadAssertionDefault = new ThreadAssertionDefault(thread, exceptionMapper);
    } else {
      threadAssertionDefault.setCandidate(thread);
    }
    return threadAssertionDefault;
  }

  @Override
  public void assertThat(boolean assertion, String message) {

    if (!assertion) {
      throw exceptionMapper.apply(message);
    }
  }
}
