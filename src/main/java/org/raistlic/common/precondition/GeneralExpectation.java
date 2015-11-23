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

import org.raistlic.common.predicate.Predicates;

import java.util.function.Function;
import java.util.function.Predicate;

/**
 * The class helps to check a specified {@code candidate} with certain expectations, and throws
 * a custom runtime exception when the check fails.
 *
 * @author Lei Chen (2015-10-14)
 */
public class GeneralExpectation<E> extends AbstractExpectation<E> {

  private final String name;

  GeneralExpectation(E candidate,
                     String name,
                     Function<String, ? extends RuntimeException> exceptionProvider) {

    super(candidate, exceptionProvider);
    this.name = name;
  }

  String name() {

    return name;
  }

  /**
   * The method claims that the {@code candidate} should be {@code null}, otherwise a runtime
   * exception will be thrown.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  public void isNull() {

    String message = "";
    if (name != null) {
      message = "'" + name + "' should be null, but was " + super.getCandidate();
    }
    isNull(message);
  }

  /**
   * The method claims that the {@code candidate} should be {@code null}, otherwise a runtime
   * exception with the specified {@code message} will be thrown.
   *
   * @param message the message to be thrown with the exception, in case the check fails.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  public void isNull(String message) {

    setMessage(message);
    setPredicate(Predicates.isNull());
    evaluate();
  }

  /**
   * The method claims that the {@code candidate} should not be {@code null}, otherwise a runtime
   * exception will be thrown.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  public void notNull() {

    String message = "";
    if (name != null) {
      message = "'" + name + "' should not be null.";
    }
    notNull(message);
  }

  /**
   * The method claims that the {@code candidate} should not be {@code null}, otherwise a runtime
   * exception with the specified {@code message} will be thrown.
   *
   * @param message the message to be thrown with the exception, in case the check fails.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  public void notNull(String message) {

    setMessage(message);
    setPredicate(Predicates.notNull());
    evaluate();
  }

  /**
   * The method claims that the {@code candidate} should be equal to the {@code target}, otherwise a
   * runtime exception will be thrown.
   *
   * @param target the reference target which the candidate should be equal to, or {@code null} if
   *               the candidate should be {@code null}.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  public void equalTo(E target) {

    String message = "";
    if (name != null) {
      message = "'" + name + "' should be equal to '" + target + "', but is not.";
    }
    equalTo(target, message);
  }

  /**
   * The method claims that the {@code candidate} should be equal to the {@code target}, otherwise a
   * runtime exception with the specified {@code message} will be thrown.
   *
   * @param target the reference target which the candidate should be equal to, or {@code null} if
   *               the candidate should be {@code null}.
   * @param message the message to be thrown with the exception, in case the check fails.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  public void equalTo(E target, String message) {

    setMessage(message);
    setPredicate(Predicates.equalTo(target));
    evaluate();
  }

  /**
   * The method claims that the {@code candidate} should NOT be equal to the {@code target},
   * otherwise a runtime exception will be thrown.
   *
   * @param target the reference target which the candidate should be equal to, or {@code null} if
   *               the candidate should be {@code null}.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  public void notEqualTo(E target) {

    String message = "";
    if (name != null) {
      message = "'" + name + "' should not be equal to '" + target + "'.";
    }
    notEqualTo(target, message);
  }

  /**
   * The method claims that the {@code candidate} should NOT be equal to the {@code target},
   * otherwise a runtime exception with the specified {@code message} will be thrown.
   *
   * @param target the reference target which the candidate should be equal to, or {@code null} if
   *               the candidate should be {@code null}.
   * @param message the message to be thrown with the exception, in case the check fails.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  public void notEqualTo(E target, String message) {

    setMessage(message);
    setPredicate(Predicates.notEqualTo(target));
    evaluate();
  }

  /**
   * The methods claims that the {@code candidate} should match the specified {@code predicate} ,
   * otherwise a runtime exception will be thrown.
   *
   * @param predicate the predicate that's used to test the {@code candidate} , cannot be {@code null}.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  public void matches(Predicate<? super E> predicate) {

    String message = "";
    if (name != null) {
      message = "'" + name + "' does not match the specified predicate.";
    }
    matches(predicate, message);
  }

  /**
   * The methods claims that the {@code candidate} should match the specified {@code predicate} ,
   * otherwise a runtime exception with the {@code message} will be thrown.
   *
   * @param predicate the predicate that's used to test the {@code candidate} , cannot be {@code null}.
   * @param message the message to be thrown with the exception, in case the check fails.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  public void matches(Predicate<? super E> predicate, String message) {

    Precondition.param(predicate, "predicate").notNull();

    setMessage(message);
    setPredicate(predicate);
    evaluate();
  }
}
