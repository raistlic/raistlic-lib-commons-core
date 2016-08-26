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
import org.raistlic.common.expectation.ExpectedCases;
import org.raistlic.common.expectation.ExpectedCasesFactory;
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

  /**
   * The method sets a mapper for parameter checks for creating exception from {@link String} message when needed.
   *
   * @param exceptionMapper the exception mapper to set, cannot be {@code null}.
   *
   * @throws InvalidParameterException or an exception of the currently configured exception type for parameter checks
   *         failure when {@code exceptionMapper} is {@code null}.
   */
  public static void setParameterExceptionMapper(Function<String, ? extends RuntimeException> exceptionMapper) {

    assertParam(exceptionMapper != null);

    paramExpectedCasesFactory.setExceptionMapper(exceptionMapper)
        .ifPresent(paramExpectedCasesReference::set);
  }

  /**
   * The method sets a mapper for context checks for creating exception from {@link String} message when needed.
   *
   * @param exceptionMapper the exception mapper to set, cannot be {@code null}.
   *
   * @throws InvalidParameterException or an exception of the currently configured exception type for parameter checks
   *         failure when {@code exceptionMapper} is {@code null}. See also {@link #setParameterExceptionMapper(Function)}.
   */
  public static void setContextExceptionMapper(Function<String, ? extends RuntimeException> exceptionMapper) {

    assertParam(exceptionMapper != null);

    contextExpectedCasesFactory.setExceptionMapper(exceptionMapper)
        .ifPresent(contextExpectedCasesReference::set);
  }

  /**
   * The method sets the strategy of how expectation instances are managed in the {@link ExpectedCases} used for
   * the checks. This sets the given {@code strategy} to both parameter and context checks.
   *
   * @param strategy the strategy to set, cannot be {@code null}.
   *
   * @throws InvalidParameterException or an exception of the currently configured exception type for parameter checks
   *         failure when {@code strategy} is {@code null}. See also {@link #setParameterExceptionMapper(Function)}.
   */
  public static void setExpectedCasesStrategy(ExpectedCases.Strategy strategy) {

    setParameterExpectedCasesStrategy(strategy);
    setContextExpectedCasesStrategy(strategy);
  }

  /**
   * Sets the strategy of how expectation instances are managed in the {@link ExpectedCases} used for parameter
   * checks .
   *
   * @param strategy the strategy to set, cannot be {@code null}.
   *
   * @throws InvalidParameterException or an exception configured for parameter check failures when {@code strategy} is
   *         {@code null}. See also {@link #setParameterExceptionMapper(Function)}.
   */
  public static void setParameterExpectedCasesStrategy(ExpectedCases.Strategy strategy) {

    assertParam(strategy != null);

    paramExpectedCasesFactory.setExpectedCasesStrategy(strategy)
        .ifPresent(paramExpectedCasesReference::set);
  }

  /**
   * Sets the strategy of how expectation instances are managed in the {@link ExpectedCases} used for context checks.
   *
   * @param strategy the strategy to set, cannot be {@code null}.
   *
   * @throws InvalidParameterException or an exception configured for parameter check failures when {@code strategy} is
   *         {@code null}. See also {@link #setParameterExceptionMapper(Function)}.
   */
  public static void setContextExpectedCasesStrategy(ExpectedCases.Strategy strategy) {

    assertParam(strategy != null);

    contextExpectedCasesFactory.setExpectedCasesStrategy(strategy)
        .ifPresent(contextExpectedCasesReference::set);
  }

  /**
   * Turns on both parameter and context checks.
   */
  public static void switchOn() {

    switchParameterCheckOn();
    switchContextCheckOn();
  }

  /**
   * Turns on the parameter checks.
   */
  public static void switchParameterCheckOn() {

    paramExpectedCasesFactory.setSwitch(true);
  }

  /**
   * Turns on the context checks.
   */
  public static void switchContextCheckOn() {

    contextExpectedCasesFactory.setSwitch(true);
  }

  /**
   * Turns off both parameter and context checks. See also {@link #switchOn()}.
   */
  public static void switchOff() {

    switchParameterCheckOff();
    switchContextCheckOff();
  }

  /**
   * Turns off the parameter checks, so that they won't throw exception regardless of whether the expectation matches.
   * See also {@link #switchParameterCheckOn()}.
   */
  public static void switchParameterCheckOff() {

    paramExpectedCasesFactory.setSwitch(false);
  }

  /**
   * Turns off the context checks, so that they won't throw exception regardless of whether the expectation matches.
   * See also {@link #switchContextCheckOn()}.
   */
  public static void switchContextCheckOff() {

    contextExpectedCasesFactory.setSwitch(false);
  }

  private static ExpectedCases contextExpectedCases() {

    return contextExpectedCasesReference.get();
  }

  private static ExpectedCases paramExpectedCases() {

    return paramExpectedCasesReference.get();
  }

  // parameter preconditions -----------------------------------------------------------------------

  /**
   * Returns a generic expectation instance for checking the specified {@code parameter} .
   *
   * @param parameter the parameter to be checked.
   * @param <E> the actual type of the parameter.
   * @return the generic expectation instance. See also {@link GenericExpectation} .
   */
  public static <E> GenericExpectation<E> param(E parameter) {

    return paramExpectedCases().expect(parameter);
  }

  /**
   * Returns an expectation instance for checking the specified {@link String} {@code parameter} .
   *
   * @param parameter the {@link String} parameter to be checked.
   * @return a {@link String} expectation instance. See also {@link StringExpectation} .
   */
  public static StringExpectation param(String parameter) {

    return paramExpectedCases().expect(parameter);
  }

  /**
   * Returns an expectation instance for checking the specified {@link Number} {@code parameter} .
   *
   * @param parameter the {@link Number} parameter to be checked.
   * @param <N> the actual type of the {@code parameter} .
   * @return a {@link Number} expectation instance. See also {@link NumberExpectation} .
   */
  public static <N extends Number & Comparable<N>> NumberExpectation<N> param(N parameter) {

    return paramExpectedCases().expect(parameter);
  }

  /**
   * Returns an expectation instance for checking the specified {@link Boolean} {@code parameter} .
   *
   * @param parameter the {@link Boolean} parameter to be checked.
   * @return a {@link Boolean} expectation instance. See also {@link BooleanExpectation} .
   */
  public static BooleanExpectation param(Boolean parameter) {

    return paramExpectedCases().expect(parameter);
  }

  /**
   * Returns an expectation instance for checking the primitive {@code boolean} {@code parameter} .
   *
   * @param parameter the {@code boolean} parameter to be checked.
   * @return a {@code boolean} expectation instance. See also {@link PrimitiveBooleanExpectation} .
   */
  public static PrimitiveBooleanExpectation param(boolean parameter) {

    return paramExpectedCases().expect(parameter);
  }

  /**
   * Returns an expectation instance for checking the specified {@link Collection} {@code parameter} .
   *
   * @param parameter the {@link Collection} parameter to be checked.
   * @param <E> the actual element type of the {@link Collection} .
   * @return a {@link Collection} expectation instance. See also {@link CollectionExpectation} .
   */
  public static <E> CollectionExpectation<E> param(Collection<E> parameter) {

    return paramExpectedCases().expect(parameter);
  }

  /**
   * Makes an assertion about the parameter, throws appropriate exception when the specified {@code statement} is {@code false}.
   *
   * @param statement the assertion statement that's claimed to be {@code true}.
   *
   * @throws InvalidParameterException or an exception of the currently configured exception type for parameter checks
   *         failure when {@code statement} is {@code false}. See also {@link #setParameterExceptionMapper(Function)}.
   */
  public static void assertParam(boolean statement) {

    assertParam(statement, "");
  }

  /**
   * Makes an assertion about the parameter, throws appropriate exception with the specified {@code message} when
   * {@code statement} is {@code false}.
   *
   * @param statement the assertion statement that's claimed to be {@code true}.
   * @param message the message for exception when {@code statement} is {@code false}.
   *
   * @throws InvalidParameterException or an exception of the currently configured exception type for parameter checks
   *         failure when {@code statement} is {@code false}. See also {@link #setParameterExceptionMapper(Function)}.
   */
  public static void assertParam(boolean statement, String message) {

    paramExpectedCases().assertThat(statement, message);
  }

  // context preconditions -------------------------------------------------------------------------

  /**
   * Returns an expectation for checking the specified {@link Boolean} {@code contextState} .
   *
   * @param contextState the {@link Boolean} context state to be checked.
   * @return a {@link Boolean} expectation instance. See also {@link BooleanExpectation} .
   */
  public static BooleanExpectation context(Boolean contextState) {

    return contextExpectedCases().expect(contextState);
  }

  /**
   * Returns an expectation for checking the specified {@code boolean contextState} .
   *
   * @param contextState the {@code boolean} context state to be checked.
   * @return a {@code boolean} expectation instance. See also {@link PrimitiveBooleanExpectation} .
   */
  public static PrimitiveBooleanExpectation context(boolean contextState) {

    return contextExpectedCases().expect(contextState);
  }

  /**
   * Returns an expectation instance for checking the specified {@link String} {@code contextState} .
   *
   * @param contextState the context state to be checked.
   * @return a string expectation instance. See also {@link StringExpectation} .
   */
  public static StringExpectation context(String contextState) {

    return contextExpectedCases().expect(contextState);
  }

  /**
   * Returns an expectation instance for checking the specified {@code contextState} .
   *
   * @param contextState the context state to be checked.
   * @param <E> the actual type of the {@code contextState} .
   * @return a generic expectation instance. See also {@link GenericExpectation} .
   */
  public static <E> GenericExpectation<E> context(E contextState) {

    return contextExpectedCases().expect(contextState);
  }

  /**
   * Returns an expectation instance for checking the specified {@code contextState} .
   *
   * @param contextState the context state to be checked.
   * @param <N> the actual {@link Number} sub-type of the {@code contextState} .
   * @return a number expectation instance. See also {@link NumberExpectation} .
   */
  public static <N extends Number & Comparable<N>> NumberExpectation<N> context(N contextState) {

    return contextExpectedCases().expect(contextState);
  }

  /**
   * Makes an assertion about the context, throws appropriate exception when {@code statement} is {@code false}.
   *
   * @param statement the asserted statement that is claimed to be {@code true}.
   *
   * @throws InvalidContextException or an exception of the configured type when {@code statement} is {@code false}. See
   *         also {@link #setContextExceptionMapper(Function)}.
   */
  public static void assertContext(boolean statement) {

    assertContext(statement, "");
  }

  /**
   * Makes an assertion about the context, throws appropriate exception with the specified {@code message} when
   * {@code statement} is {@code false}.
   *
   * @param statement the asserted statement that is claimed to be {@code true}.
   * @param message the message for exception when {@code statement} is false.
   *
   * @throws InvalidContextException or an exception of the configured type when {@code statement} is {@code false}. See
   *         also {@link #setContextExceptionMapper(Function)}.
   */
  public static void assertContext(boolean statement, String message) {

    contextExpectedCases().assertThat(statement, message);
  }

  /**
   * Returns an expectation instance for checking the current thread.
   *
   * @return the {@link ThreadExpectation} for current thread.
   */
  public static ThreadExpectation currentThread() {

    return contextExpectedCases().expect(Thread.currentThread());
  }

  /**
   * The method checks that the current thread is the Swing event dispatch thread, or otherwise throws an exception.
   */
  public static void isInEventDispatchThread() {

    assertContext(SwingUtilities.isEventDispatchThread(),
        "Expected in EDT, but is not, thread id is: '" + Thread.currentThread().getId() + "'");
  }

  private Precondition() { }

  private static final ExpectedCasesFactory paramExpectedCasesFactory = new ExpectedCasesFactory(
      InvalidParameterException::new, ExpectedCases.Strategy.THREAD_LOCAL
  );

  private static final ExpectedCasesFactory contextExpectedCasesFactory = new ExpectedCasesFactory(
      InvalidContextException::new, ExpectedCases.Strategy.THREAD_LOCAL
  );

  private static final AtomicReference<ExpectedCases> paramExpectedCasesReference = new AtomicReference<>(
      paramExpectedCasesFactory.create()
  );

  private static final AtomicReference<ExpectedCases> contextExpectedCasesReference = new AtomicReference<>(
      contextExpectedCasesFactory.create()
  );
}
