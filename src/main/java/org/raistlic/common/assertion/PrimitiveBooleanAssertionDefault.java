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

/**
 * The class defines both expectation class for the primitive and boxed boolean values.
 *
 * @author Lei CHEN (2015-11-20)
 */
final class PrimitiveBooleanAssertionDefault implements PrimitiveBooleanAssertion {

  private final Function<String, ? extends RuntimeException> exceptionMapper;

  private boolean candidate;

  PrimitiveBooleanAssertionDefault(boolean candidate,
                                   Function<String, ? extends RuntimeException> exceptionMapper) {

    Precondition.assertParam(exceptionMapper != null, "exceptionMapper cannot be null.");

    this.candidate = candidate;
    this.exceptionMapper = exceptionMapper;
  }

  @Override
  public void isTrue() {

    if (!candidate) {
      throw exceptionMapper.apply("Candidate should be true, but is " + candidate);
    }
  }

  PrimitiveBooleanAssertion setCandidate(boolean candidate) {

    this.candidate = candidate;
    return this;
  }

  @Override
  public void isTrue(String message) {

    if (!candidate) {
      throw exceptionMapper.apply(message);
    }
  }

  @Override
  public void isFalse() {

    if (candidate) {
      throw exceptionMapper.apply("Candidate should be false, but is " + candidate);
    }
  }

  @Override
  public void isFalse(String message) {

    if (candidate) {
      throw exceptionMapper.apply(message);
    }
  }

  @Override
  public void isEqualTo(boolean expected) {

    if (candidate != expected) {
      String message = "Candidate should be equal to " + expected + ", but it's not.";
      throw exceptionMapper.apply(message);
    }
  }

  @Override
  public void isEqualTo(boolean expected, String message) {

    if (candidate != expected) {
      throw exceptionMapper.apply(message);
    }
  }
}
