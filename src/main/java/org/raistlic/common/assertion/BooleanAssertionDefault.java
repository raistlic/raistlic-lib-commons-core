/*
 * Copyright 2016 Lei CHEN (raistlic@gmail.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
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

final class BooleanAssertionDefault extends GenericAssertionAbstract<Boolean, BooleanAssertion>
  implements BooleanAssertion {

  private final Function<String, ? extends RuntimeException> exceptionMapper;

  private Boolean candidate;

  BooleanAssertionDefault(Boolean candidate, Function<String, ? extends RuntimeException> exceptionMapper) {

    Precondition.assertParam(exceptionMapper != null, "expectationMapper cannot be null.");

    this.candidate = candidate;
    this.exceptionMapper = exceptionMapper;
  }

  @Override
  final BooleanAssertion getThis() {

    return this;
  }

  BooleanAssertion setCandidate(Boolean candidate) {

    this.candidate = candidate;
    return getThis();
  }

  @Override
  Boolean getCandidate() {

    return candidate;
  }

  @Override
  Function<String, ? extends RuntimeException> getExceptionMapper() {

    return exceptionMapper;
  }

  @Override
  public void isTrue() {

    if (!Boolean.TRUE.equals(candidate)) {
      throw exceptionMapper.apply("Candidate should be true, but was " + candidate);
    }
  }

  @Override
  public void isTrue(String message) {

    if (!Boolean.TRUE.equals(candidate)) {
      throw exceptionMapper.apply(message);
    }
  }

  @Override
  public void isFalse() {

    if (!Boolean.FALSE.equals(candidate)) {
      throw exceptionMapper.apply("Candidate should be false, but was " + candidate);
    }
  }

  @Override
  public void isFalse(String message) {

    if (!Boolean.FALSE.equals(candidate)) {
      throw exceptionMapper.apply(message);
    }
  }
}
