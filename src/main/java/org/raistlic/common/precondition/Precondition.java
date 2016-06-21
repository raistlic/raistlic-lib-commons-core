/*
 * Copyright 2015 Lei CHEN (raistlic@gmail.com)
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

package org.raistlic.common.precondition;

import org.raistlic.common.expectation.BooleanExpectation;
import org.raistlic.common.expectation.CollectionExpectation;
import org.raistlic.common.expectation.Expectations;
import org.raistlic.common.expectation.ExpectedCases;
import org.raistlic.common.expectation.GenericExpectation;
import org.raistlic.common.expectation.NumberExpectation;
import org.raistlic.common.expectation.PrimitiveBooleanExpectation;
import org.raistlic.common.expectation.StringExpectation;
import org.raistlic.common.expectation.ThreadExpectation;

import javax.swing.SwingUtilities;
import java.util.Collection;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

/**
 * The class is used as the entry point for precondition checks, it has utility methods for
 * validation work, as well as some static factory methods that expose proper {@link ExpectedCases}
 * instances for different types of objects.
 *
 * @author Lei CHEN (2015-02-13)
 * @since 1.2
 */
public final class Precondition {

  // configuration ---------------------------------------------------------------------------------

  public static void setParameterExceptionMapper(Function<String, ? extends RuntimeException> exceptionMapper) {

    param(exceptionMapper).isNotNull();

    PARAMETER_EXPECTED_CASES.set(EXPECTED_CASES_FACTORY.apply(exceptionMapper));
  }

  private static ExpectedCases paramExpectedCases() {

    return PARAMETER_EXPECTED_CASES.get();
  }

  public static void setContextExceptionMapper(Function<String, ? extends RuntimeException> exceptionMapper) {

    param(exceptionMapper).isNotNull();

    CONTEXT_EXPECTED_CASES.set(EXPECTED_CASES_FACTORY.apply(exceptionMapper));
  }

  private static ExpectedCases contextExpectedCases() {

    return CONTEXT_EXPECTED_CASES.get();
  }

  // parameter preconditions -----------------------------------------------------------------------

  public static <E> GenericExpectation<E> param(E parameter) {

    return paramExpectedCases().expect(parameter);
  }

  public static StringExpectation param(String parameter) {

    return paramExpectedCases().expect(parameter);
  }

  public static <N extends Number & Comparable<N>> NumberExpectation<N> param(N parameter) {

    return paramExpectedCases().expect(parameter);
  }

  public static BooleanExpectation param(Boolean parameter) {

    return paramExpectedCases().expect(parameter);
  }

  public static PrimitiveBooleanExpectation param(boolean parameter) {

    return paramExpectedCases().expect(parameter);
  }

  public static <E> CollectionExpectation<E> param(Collection<E> parameter) {

    return paramExpectedCases().expect(parameter);
  }

  public static void assertParam(boolean statement) {

    assertParam(statement, "");
  }

  public static void assertParam(boolean statement, String message) {

    paramExpectedCases().assertThat(statement, message);
  }

  // context preconditions -------------------------------------------------------------------------

  public static BooleanExpectation context(Boolean contextState) {

    return contextExpectedCases().expect(contextState);
  }

  public static PrimitiveBooleanExpectation context(boolean contextState) {

    return contextExpectedCases().expect(contextState);
  }

  public static StringExpectation context(String contextState) {

    return contextExpectedCases().expect(contextState);
  }

  public static <E> GenericExpectation<E> context(E contextState) {

    return contextExpectedCases().expect(contextState);
  }

  public static <N extends Number & Comparable<N>> NumberExpectation<N> context(N contextState) {

    return contextExpectedCases().expect(contextState);
  }

  public static void assertContext(boolean statement) {

    assertContext(statement, "");
  }

  public static void assertContext(boolean statement, String message) {

    contextExpectedCases().assertThat(statement, message);
  }

  public static ThreadExpectation currentThread() {

    return contextExpectedCases().expect(Thread.currentThread());
  }

  public static void isInEventDispatchThread() {

    assertContext(SwingUtilities.isEventDispatchThread(),
        "Expected in EDT, but is not, thread id is: '" + Thread.currentThread().getId() + "'");
  }

  private Precondition() { }

  private static final Function<Function<String, ? extends RuntimeException>, ExpectedCases> EXPECTED_CASES_FACTORY =
      Expectations::createDefaultExpectedCases;

  private static final AtomicReference<ExpectedCases> PARAMETER_EXPECTED_CASES = new AtomicReference<>(
      EXPECTED_CASES_FACTORY.apply(ExceptionProviders.invalidParameterExceptionProvider())
  );

  private static final AtomicReference<ExpectedCases> CONTEXT_EXPECTED_CASES = new AtomicReference<>(
      EXPECTED_CASES_FACTORY.apply(ExceptionProviders.invalidContextExceptionProvider())
  );
}
