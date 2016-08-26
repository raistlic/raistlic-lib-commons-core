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

import org.raistlic.common.precondition.Precondition;

import java.util.function.Function;

final class GenericExpectationDefault<E> extends GenericExpectationAbstract<E, GenericExpectation<E>>
    implements GenericExpectation<E> {

  private final E candidate;

  private final Function<String, ? extends RuntimeException> exceptionProvider;

  GenericExpectationDefault(E candidate, Function<String, ? extends RuntimeException> exceptionProvider) {

    Precondition.assertParam(exceptionProvider != null, "'exceptionProvider' should not be null, but it is.");

    this.candidate = candidate;
    this.exceptionProvider = exceptionProvider;
  }

  @Override
  GenericExpectation<E> getThis() {

    return this;
  }

  @Override
  E getCandidate() {

    return candidate;
  }

  @Override
  Function<String, ? extends RuntimeException> getExceptionMapper() {

    return exceptionProvider;
  }
}
