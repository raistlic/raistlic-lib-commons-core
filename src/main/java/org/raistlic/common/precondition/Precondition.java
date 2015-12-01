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

/**
 * @author Lei CHEN (2015-02-13)
 * @since 1.2
 */
public final class Precondition {

  // parameter preconditions -----------------------------------------------------------------------

  public static <E> GeneralExpectation<E> param(E parameter) {

    return param(parameter, null);
  }

  public static <E> GeneralExpectation<E> param(E parameter, String name) {

    return new GeneralExpectation<E>(
        parameter, name, ExceptionProviders.invalidParameterExceptionProvider());
  }

  public static StringExpectation param(String parameter) {

    return param(parameter, null);
  }

  public static StringExpectation param(String parameter, String name) {

    return new StringExpectation(
        parameter, name, ExceptionProviders.invalidParameterExceptionProvider());
  }

  public static <N extends Number & Comparable<N>> NumberExpectation<N> param(N parameter) {

    return param(parameter, null);
  }

  public static <N extends Number & Comparable<N>> NumberExpectation<N> param(N parameter, String name) {

    return new NumberExpectation<N>(
        parameter, name, ExceptionProviders.invalidParameterExceptionProvider());
  }

  public static BooleanExpectation.Boxed param(Boolean parameter) {

    return param(parameter, null);
  }

  public static BooleanExpectation.Boxed param(Boolean parameter, String name) {

    return new BooleanExpectation.Boxed(
            parameter, name, ExceptionProviders.invalidParameterExceptionProvider());
  }

  public static BooleanExpectation.Primitive param(boolean parameter) {

    return param(parameter, null);
  }

  public static BooleanExpectation.Primitive param(boolean parameter, String name) {

    return new BooleanExpectation.Primitive(
            parameter, name, ExceptionProviders.invalidParameterExceptionProvider());
  }

  public static void assertParam(boolean parameter) {

    assertParam(parameter, "");
  }

  public static void assertParam(boolean parameter, String message) {

    if (!parameter) {
      throw new InvalidParameterException(message);
    }
  }

  // state preconditions ---------------------------------------------------------------------------

  public static <E> GeneralExpectation<E> state(E state) {

    return state(state, null);
  }

  public static <E> GeneralExpectation<E> state(E state, String name) {

    return new GeneralExpectation<E>(state, name, ExceptionProviders.invalidStateExceptionProvider());
  }

  public static StringExpectation state(String state) {

    return state(state, null);
  }

  public static StringExpectation state(String state, String name) {

    return new StringExpectation(state, name, ExceptionProviders.invalidStateExceptionProvider());
  }

  public static <N extends Number & Comparable<N>> NumberExpectation<N> state(N state) {

    return state(state, null);
  }

  public static <N extends Number & Comparable<N>> NumberExpectation<N> state(N state, String name) {

    return new NumberExpectation<N>(state, name, ExceptionProviders.invalidStateExceptionProvider());
  }

  public static BooleanExpectation.Boxed state(Boolean state) {

    return state(state, null);
  }

  public static BooleanExpectation.Boxed state(Boolean state, String name) {

    return new BooleanExpectation.Boxed(state, name, ExceptionProviders.invalidStateExceptionProvider());
  }

  public static BooleanExpectation.Primitive state(boolean state) {

    return state(state, null);
  }

  public static BooleanExpectation.Primitive state(boolean state, String name) {

    return new BooleanExpectation.Primitive(state, name, ExceptionProviders.invalidStateExceptionProvider());
  }

  public static void assertState(boolean statement) {

    assertState(statement, "");
  }

  public static void assertState(boolean statement, String message) {

    if (!statement) {
      throw new InvalidStateException(message);
    }
  }

  // context preconditions -------------------------------------------------------------------------

  public static ThreadExpectation threadContext() {

    return new ThreadExpectation(
            Thread.currentThread(),
            ExceptionProviders.invalidContextExceptionProvider()
    );
  }

  private Precondition() { }
}
