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
import org.raistlic.common.precondition.PreconditionCheckFailedException;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Function;

/**
 * Static factory methods holder, the entry point of the package.
 *
 * @author Lei Chen (2015-12-29)
 */
public final class Assertions {

  public static AssertionFactoryManager createAssertionFactoryManager(
      Function<String, ? extends RuntimeException> exceptionMapper) {

    if (exceptionMapper == null) {
      throw new PreconditionCheckFailedException("'exceptionMapper' cannot be null.");
    }
    return new AssertionFactoryManager(exceptionMapper);
  }

  /**
   * Creates and returns a new instance of {@link AssertionFactory} with the specified exception mapper.
   *
   * @param exceptionMapper the exception mapper to use, cannot be {@code null}.
   * @return the created {@link AssertionFactory} instance.
   *
   * @throws InvalidParameterException when {@code exceptionMapper} is {@code null}.
   */
  public static AssertionFactory createDefaultExpectedCases(Function<String, ? extends RuntimeException> exceptionMapper) {

    return new AssertionFactoryDefault(exceptionMapper);
  }

  /**
   * Creates and returns a new instance of {@link AssertionFactory} with the specified exception mapper, which reuses
   * thread local expectation instances on invocation.
   *
   * @param exceptionMapper the exception mapper that creates proper exception with message when needed.
   * @return the created {@link AssertionFactory} instance.
   *
   * @throws InvalidParameterException when {@code exceptionMapper} is {@code null}.
   */
  public static AssertionFactory createThreadLocalExpectedCases(Function<String, ? extends RuntimeException> exceptionMapper) {

    return new AssertionFactoryThreadLocal(exceptionMapper);
  }

  public static AssertionFactory createSwitchableProxy(AssertionFactory assertionFactory, AtomicBoolean theSwitch) {

    return new AssertionFactorySwitchableProxy(assertionFactory, theSwitch);
  }

  /*
   * Static method holder, not to be instanticated or inherited.
   */
  private Assertions() { }
}
