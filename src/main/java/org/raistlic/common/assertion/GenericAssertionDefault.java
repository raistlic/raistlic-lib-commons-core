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

import org.raistlic.common.precondition.Precondition;

import java.util.function.Function;

final class GenericAssertionDefault<E> extends GenericAssertionAbstract<E, GenericAssertion<E>>
  implements GenericAssertion<E> {

  private E candidate;

  private final Function<String, ? extends RuntimeException> exceptionProvider;

  GenericAssertionDefault(E candidate, Function<String, ? extends RuntimeException> exceptionProvider) {

    Precondition.assertParam(exceptionProvider != null, "'exceptionProvider' should not be null, but it is.");

    this.candidate = candidate;
    this.exceptionProvider = exceptionProvider;
  }

  @Override
  GenericAssertion<E> getThis() {

    return this;
  }

  @Override
  E getCandidate() {

    return candidate;
  }

  void setCandidate(E candidate) {

    this.candidate = candidate;
  }

  @Override
  Function<String, ? extends RuntimeException> getExceptionMapper() {

    return exceptionProvider;
  }
}
