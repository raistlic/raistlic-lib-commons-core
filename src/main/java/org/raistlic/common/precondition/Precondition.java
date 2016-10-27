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

import org.raistlic.common.assertion.AssertionFactory;
import org.raistlic.common.assertion.AssertionFactoryManager;
import org.raistlic.common.assertion.Assertions;
import org.raistlic.common.assertion.BooleanAssertion;
import org.raistlic.common.assertion.CollectionAssertion;
import org.raistlic.common.assertion.GenericAssertion;
import org.raistlic.common.assertion.NumberAssertion;
import org.raistlic.common.assertion.PrimitiveBooleanAssertion;
import org.raistlic.common.assertion.StringAssertion;
import org.raistlic.common.assertion.ThreadAssertion;

import javax.swing.SwingUtilities;
import java.util.Collection;
import java.util.function.Function;

/**
 * The class is used as the entry point for precondition checks, it has utility methods for
 * validation work, as well as some static factory methods that expose proper {@link AssertionFactory}
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

    paramAssertionFactoryManager.setExceptionMapper(exceptionMapper);
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

    contextAssertionFactoryManager.setExceptionMapper(exceptionMapper);
  }

  /**
   * The method sets the strategy of how expectation instances are managed in the {@link AssertionFactory} used for
   * the checks. This sets the given {@code strategy} to both parameter and context checks.
   *
   * @param strategy the strategy to set, cannot be {@code null}.
   *
   * @throws InvalidParameterException or an exception of the currently configured exception type for parameter checks
   *         failure when {@code strategy} is {@code null}. See also {@link #setParameterExceptionMapper(Function)}.
   */
  public static void setExpectedCasesStrategy(AssertionFactory.Strategy strategy) {

    setParameterExpectedCasesStrategy(strategy);
    setContextExpectedCasesStrategy(strategy);
  }

  /**
   * Sets the strategy of how expectation instances are managed in the {@link AssertionFactory} used for parameter
   * checks .
   *
   * @param strategy the strategy to set, cannot be {@code null}.
   *
   * @throws InvalidParameterException or an exception configured for parameter check failures when {@code strategy} is
   *         {@code null}. See also {@link #setParameterExceptionMapper(Function)}.
   */
  public static void setParameterExpectedCasesStrategy(AssertionFactory.Strategy strategy) {

    paramAssertionFactoryManager.setStrategy(strategy);
  }

  /**
   * Sets the strategy of how expectation instances are managed in the {@link AssertionFactory} used for context checks.
   *
   * @param strategy the strategy to set, cannot be {@code null}.
   *
   * @throws InvalidParameterException or an exception configured for parameter check failures when {@code strategy} is
   *         {@code null}. See also {@link #setParameterExceptionMapper(Function)}.
   */
  public static void setContextExpectedCasesStrategy(AssertionFactory.Strategy strategy) {

    contextAssertionFactoryManager.setStrategy(strategy);
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

    paramAssertionFactoryManager.switchOn();
  }

  /**
   * Turns on the context checks.
   */
  public static void switchContextCheckOn() {

    contextAssertionFactoryManager.switchOn();
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

    paramAssertionFactoryManager.switchOff();
  }

  /**
   * Turns off the context checks, so that they won't throw exception regardless of whether the expectation matches.
   * See also {@link #switchContextCheckOn()}.
   */
  public static void switchContextCheckOff() {

    contextAssertionFactoryManager.switchOff();
  }

  // parameter preconditions -----------------------------------------------------------------------

  /**
   * Returns a generic expectation instance for checking the specified {@code parameter} .
   *
   * @param parameter the parameter to be checked.
   * @param <E> the actual type of the parameter.
   * @return the generic expectation instance. See also {@link GenericAssertion} .
   */
  public static <E> GenericAssertion<E> param(E parameter) {

    return paramExpectedCases().expect(parameter);
  }

  /**
   * Returns an expectation instance for checking the specified {@link String} {@code parameter} .
   *
   * @param parameter the {@link String} parameter to be checked.
   * @return a {@link String} expectation instance. See also {@link StringAssertion} .
   */
  public static StringAssertion param(String parameter) {

    return paramExpectedCases().expect(parameter);
  }

  /**
   * Returns an expectation instance for checking the specified {@link Number} {@code parameter} .
   *
   * @param parameter the {@link Number} parameter to be checked.
   * @param <N> the actual type of the {@code parameter} .
   * @return a {@link Number} expectation instance. See also {@link NumberAssertion} .
   */
  public static <N extends Number & Comparable<N>> NumberAssertion<N> param(N parameter) {

    return paramExpectedCases().expect(parameter);
  }

  /**
   * Returns an expectation instance for checking the specified {@link Boolean} {@code parameter} .
   *
   * @param parameter the {@link Boolean} parameter to be checked.
   * @return a {@link Boolean} expectation instance. See also {@link BooleanAssertion} .
   */
  public static BooleanAssertion param(Boolean parameter) {

    return paramExpectedCases().expect(parameter);
  }

  /**
   * Returns an expectation instance for checking the primitive {@code boolean} {@code parameter} .
   *
   * @param parameter the {@code boolean} parameter to be checked.
   * @return a {@code boolean} expectation instance. See also {@link PrimitiveBooleanAssertion} .
   */
  public static PrimitiveBooleanAssertion param(boolean parameter) {

    return paramExpectedCases().expect(parameter);
  }

  /**
   * Returns an expectation instance for checking the specified {@link Collection} {@code parameter} .
   *
   * @param parameter the {@link Collection} parameter to be checked.
   * @param <E> the actual element type of the {@link Collection} .
   * @return a {@link Collection} expectation instance. See also {@link CollectionAssertion} .
   */
  public static <E> CollectionAssertion<E> param(Collection<E> parameter) {

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
   * @return a {@link Boolean} expectation instance. See also {@link BooleanAssertion} .
   */
  public static BooleanAssertion context(Boolean contextState) {

    return contextExpectedCases().expect(contextState);
  }

  /**
   * Returns an expectation for checking the specified {@code boolean contextState} .
   *
   * @param contextState the {@code boolean} context state to be checked.
   * @return a {@code boolean} expectation instance. See also {@link PrimitiveBooleanAssertion} .
   */
  public static PrimitiveBooleanAssertion context(boolean contextState) {

    return contextExpectedCases().expect(contextState);
  }

  /**
   * Returns an expectation instance for checking the specified {@link String} {@code contextState} .
   *
   * @param contextState the context state to be checked.
   * @return a string expectation instance. See also {@link StringAssertion} .
   */
  public static StringAssertion context(String contextState) {

    return contextExpectedCases().expect(contextState);
  }

  /**
   * Returns an expectation instance for checking the specified {@code contextState} .
   *
   * @param contextState the context state to be checked.
   * @param <E> the actual type of the {@code contextState} .
   * @return a generic expectation instance. See also {@link GenericAssertion} .
   */
  public static <E> GenericAssertion<E> context(E contextState) {

    return contextExpectedCases().expect(contextState);
  }

  /**
   * Returns an expectation instance for checking the specified {@code contextState} .
   *
   * @param contextState the context state to be checked.
   * @param <N> the actual {@link Number} sub-type of the {@code contextState} .
   * @return a number expectation instance. See also {@link NumberAssertion} .
   */
  public static <N extends Number & Comparable<N>> NumberAssertion<N> context(N contextState) {

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
   * @return the {@link ThreadAssertion} for current thread.
   */
  public static ThreadAssertion currentThread() {

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

  private static AssertionFactory contextExpectedCases() {

    return contextAssertionFactoryManager.getCurrentFactory();
  }

  private static AssertionFactory paramExpectedCases() {

    return paramAssertionFactoryManager.getCurrentFactory();
  }

  private static final AssertionFactoryManager paramAssertionFactoryManager =
      Assertions.createAssertionFactoryManager(InvalidParameterException::new);

  private static final AssertionFactoryManager contextAssertionFactoryManager =
      Assertions.createAssertionFactoryManager(InvalidContextException::new);
}
