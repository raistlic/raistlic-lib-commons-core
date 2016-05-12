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

package org.raistlic.common.expectation;

import org.raistlic.common.precondition.Precondition;
import org.raistlic.common.predicate.Predicates;

import java.util.function.Function;
import java.util.function.Predicate;

/**
 * The class helps to check a specified {@code candidate} with certain expectations, and throws
 * a custom runtime exception when the check fails.
 *
 * @author Lei Chen (2015-10-14)
 */
public abstract class AbstractGenericExpectation<E, GE extends AbstractGenericExpectation<E, GE>> extends AbstractExpectation<E, GE> {

  private final String name;

  AbstractGenericExpectation(E candidate,
                             String name,
                             Function<String, ? extends RuntimeException> exceptionProvider) {

    super(candidate, exceptionProvider);
    this.name = name;
  }

  String name() {

    return name;
  }

  String candidateForMessage() {

    return name == null ? "'" + String.valueOf(getCandidate()) + "' " : "'" + name + "' ";
  }

  String nameForMessage() {

    return name == null ? "(unnamed candidate) " : "'" + name + "' ";
  }

  /**
   * The method claims that the {@code candidate} should be {@code null}, otherwise a runtime
   * exception will be thrown.
   *
   * @return the expectation instance itself, for method calling chain.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  public GE isNull() {

    String message = candidateForMessage() + "is (unexpectedly) not null.";
    return isNull(message);
  }

  /**
   * The method claims that the {@code candidate} should be {@code null}, otherwise a runtime
   * exception with the specified {@code message} will be thrown.
   *
   * @param message the message to be thrown with the exception, in case the check fails.
   * @return the expectation instance itself, for method calling chain.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  public GE isNull(String message) {

    setMessage(message);
    setPredicate(Predicates.isNull());
    return evaluate();
  }

  /**
   * The method claims that the {@code candidate} should not be {@code null}, otherwise a runtime
   * exception will be thrown.
   *
   * @return the expectation instance itself, for method calling chain.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  public GE isNotNull() {

    String message = nameForMessage() + "is (unexpectedly) null.";
    return isNotNull(message);
  }

  /**
   * The method claims that the {@code candidate} should not be {@code null}, otherwise a runtime
   * exception with the specified {@code message} will be thrown.
   *
   * @param message the message to be thrown with the exception, in case the check fails.
   * @return the expectation instance itself, for method calling chain.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  public GE isNotNull(String message) {

    setMessage(message);
    setPredicate(Predicates.notNull());
    return evaluate();
  }

  /**
   * The method claims that the {@code candidate} should be equal to the {@code target}, otherwise a
   * runtime exception will be thrown.
   *
   * @param target the reference target which the candidate should be equal to, or {@code null} if
   *               the candidate should be {@code null}.
   * @return the expectation instance itself, for method calling chain.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  public GE isEqualTo(E target) {

    String message = candidateForMessage() + "is (unexpectedly) not equal to '" + target + "'.";
    return isEqualTo(target, message);
  }

  /**
   * The method claims that the {@code candidate} should be equal to the {@code target}, otherwise a
   * runtime exception with the specified {@code message} will be thrown.
   *
   * @param target the reference target which the candidate should be equal to, or {@code null} if
   *               the candidate should be {@code null}.
   * @param message the message to be thrown with the exception, in case the check fails.
   * @return the expectation instance itself, for method calling chain.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  public GE isEqualTo(E target, String message) {

    setMessage(message);
    setPredicate(Predicates.equalTo(target));
    return evaluate();
  }

  /**
   * The method claims that the {@code candidate} should NOT be equal to the {@code target},
   * otherwise a runtime exception will be thrown.
   *
   * @param target the reference target which the candidate should be equal to, or {@code null} if
   *               the candidate should be {@code null}.
   * @return the expectation instance itself, for method calling chain.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  public GE isNotEqualTo(E target) {

    String message = candidateForMessage() + "is (unexpectedly) equal to '" + target + "'.";
    return isNotEqualTo(target, message);
  }

  /**
   * The method claims that the {@code candidate} should NOT be equal to the {@code target},
   * otherwise a runtime exception with the specified {@code message} will be thrown.
   *
   * @param target the reference target which the candidate should be equal to, or {@code null} if
   *               the candidate should be {@code null}.
   * @param message the message to be thrown with the exception, in case the check fails.
   * @return the expectation instance itself, for method calling chain.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  public GE isNotEqualTo(E target, String message) {

    setMessage(message);
    setPredicate(Predicates.notEqualTo(target));
    return evaluate();
  }

  /**
   * The method claims that the {@code candidate} should be an instance of the specified {@code type} ,
   * otherwise a runtime exception will be thrown.
   *
   * @param type the type of which the {@code candidate} claims to be.
   * @return the expectation instance itself, for method calling chain.
   */
  public GE isInstanceOf(Class<?> type) {

    String message = candidateForMessage() + "should be instance of type '" + type + "', but is " +
        (getCandidate() == null ? "null" : "not (actual type: '" + getCandidate().getClass().getName() + "')");
    return isInstanceOf(type, message);
  }

  /**
   * The method claims that the {@code candidate} should be an instance of the specified {@code type} ,
   * otherwise a runtime exception with the specified {@code message} will be thrown.
   *
   * @param type the type of which the {@code candidate} claims to be.
   * @param message the message to be thrown with the exception, in case the check fails.
   * @return the expectation instance itself, for method calling chain.
   */
  public GE isInstanceOf(Class<?> type, String message) {

    setMessage(message);
    setPredicate(Predicates.instanceOf(type));
    return evaluate();
  }

  /**
   * The methods claims that the {@code candidate} should match the specified {@code predicate} ,
   * otherwise a runtime exception will be thrown.
   *
   * @param predicate the predicate that's used to test the {@code candidate} , cannot be {@code null}.
   * @return the expectation instance itself, for method calling chain.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  public GE matches(Predicate<? super E> predicate) {

    String message = candidateForMessage() + "does not match the specified predicate: '" + predicate + "'";
    return matches(predicate, message);
  }

  /**
   * The methods claims that the {@code candidate} should match the specified {@code predicate} ,
   * otherwise a runtime exception with the {@code message} will be thrown.
   *
   * @param predicate the predicate that's used to test the {@code candidate} , cannot be {@code null}.
   * @param message the message to be thrown with the exception, in case the check fails.
   * @return the expectation instance itself, for method calling chain.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  public GE matches(Predicate<? super E> predicate, String message) {

    Precondition.param(predicate).isNotNull();

    setMessage(message);
    setPredicate(predicate);
    return evaluate();
  }
}
