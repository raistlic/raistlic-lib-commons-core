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
import org.raistlic.common.expectation.GeneralExpectation;
import org.raistlic.common.expectation.NumberExpectation;
import org.raistlic.common.expectation.StringExpectation;
import org.raistlic.common.expectation.ThreadExpectation;

import java.util.Collection;

/**
 * The class is used as the entry point for precondition checks, it has utility methods for
 * validation work, as well as some static factory methods that expose proper {@link ExpectedCases}
 * instances for different types of objects.
 *
 * @author Lei CHEN (2015-02-13)
 * @since 1.2
 */
public final class Precondition {

  // parameter preconditions -----------------------------------------------------------------------

  public static <E> GeneralExpectation<E> param(E parameter) {

    return param(parameter, null);
  }

  public static <E> GeneralExpectation<E> param(E parameter, String name) {

    return PARAMETER_EXPECTED_CASES.expect(parameter, name);
  }

  public static StringExpectation param(String parameter) {

    return param(parameter, null);
  }

  public static StringExpectation param(String parameter, String name) {

    return PARAMETER_EXPECTED_CASES.expect(parameter, name);
  }

  public static <N extends Number & Comparable<N>> NumberExpectation<N> param(N parameter) {

    return param(parameter, null);
  }

  public static <N extends Number & Comparable<N>> NumberExpectation<N> param(N parameter, String name) {

    return PARAMETER_EXPECTED_CASES.expect(parameter, name);
  }

  public static BooleanExpectation.Boxed param(Boolean parameter) {

    return param(parameter, null);
  }

  public static BooleanExpectation.Boxed param(Boolean parameter, String name) {

    return PARAMETER_EXPECTED_CASES.expect(parameter, name);
  }

  public static BooleanExpectation.Primitive param(boolean parameter) {

    return param(parameter, null);
  }

  public static BooleanExpectation.Primitive param(boolean parameter, String name) {

    return PARAMETER_EXPECTED_CASES.expect(parameter, name);
  }

  public static <E> CollectionExpectation<E> param(Collection<E> parameter) {

    return param(parameter, null);
  }

  public static <E> CollectionExpectation<E> param(Collection<E> parameter, String name) {

    return PARAMETER_EXPECTED_CASES.expect(parameter, name);
  }

  public static void assertParam(boolean statement) {

    assertParam(statement, "");
  }

  public static void assertParam(boolean statement, String message) {

    PARAMETER_EXPECTED_CASES.assertThat(statement, message);
  }

  // state preconditions ---------------------------------------------------------------------------

  public static <E> GeneralExpectation<E> state(E state) {

    return state(state, null);
  }

  public static <E> GeneralExpectation<E> state(E state, String name) {

    return STATE_EXPECTED_CASES.expect(state, name);
  }

  public static StringExpectation state(String state) {

    return state(state, null);
  }

  public static StringExpectation state(String state, String name) {

    return STATE_EXPECTED_CASES.expect(state, name);
  }

  public static <N extends Number & Comparable<N>> NumberExpectation<N> state(N state) {

    return state(state, null);
  }

  public static <N extends Number & Comparable<N>> NumberExpectation<N> state(N state, String name) {

    return STATE_EXPECTED_CASES.expect(state, name);
  }

  public static BooleanExpectation.Boxed state(Boolean state) {

    return state(state, null);
  }

  public static BooleanExpectation.Boxed state(Boolean state, String name) {

    return STATE_EXPECTED_CASES.expect(state, name);
  }

  public static BooleanExpectation.Primitive state(boolean state) {

    return state(state, null);
  }

  public static BooleanExpectation.Primitive state(boolean state, String name) {

    return STATE_EXPECTED_CASES.expect(state, name);
  }

  public static void assertState(boolean statement) {

    assertState(statement, "");
  }

  public static void assertState(boolean statement, String message) {

    STATE_EXPECTED_CASES.assertThat(statement, message);
  }

  // context preconditions -------------------------------------------------------------------------

  public static ThreadExpectation threadContext() {

    return new ThreadExpectation(
            Thread.currentThread(),
            ExceptionProviders.invalidContextExceptionProvider()
    );
  }

  private Precondition() { }

  private static final ExpectedCases PARAMETER_EXPECTED_CASES = Expectations.with(
          ExceptionProviders.invalidParameterExceptionProvider()
  );

  private static final ExpectedCases STATE_EXPECTED_CASES = Expectations.with(
          ExceptionProviders.invalidStateExceptionProvider()
  );
}
