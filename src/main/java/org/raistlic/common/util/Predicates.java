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
package org.raistlic.common.util;

import org.raistlic.common.precondition.Precondition;

import java.util.function.Predicate;

/**
 * The class is a collection of static factory methods that creates and exports different types of 
 * {@link Predicate} instances.
 *
 * @author Lei CHEN
 * @since 1.3
 */
public final class Predicates {

  /**
   * The method returns an immutable singleton {@link Predicate} instance that always returns
   * {@code true} for all tests regardless of what the target is.
   *
   * @param <E> the actual type of targets to test.
   * @return the singleton {@link Predicate} instance.
   */
  @SuppressWarnings({"unchecked", "rawtypes"})
  public static <E> Predicate<E> dummyTrue() {
    
    return (Predicate<E>) DummyPredicate.TRUE;
  }

  /**
   * The method returns an immutable singleton {@link Predicate} instance that always returns
   * {@code false} for all tests regardless of what the target is.
   *
   * @param <E> the actual type of targets to test.
   * @return the singleton {@link Predicate} instance.
   */
  @SuppressWarnings({"unchecked", "rawtypes"})
  public static <E> Predicate<E> dummyFalse() {
    
    return (Predicate<E>) DummyPredicate.FALSE;
  }

  /**
   * The method creates a wrapper for the specified {@code predicate} , which returns the inverse
   * result of the predicate's test.
   *
   * @param predicate the predicate to be wrapped, cannot be {@code null}.
   * @param <E> the actual type of targets to test.
   * @return the wrapper created.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code predicate} is
   *         {@code null}.
   */
  public static <E> Predicate<E> not(Predicate<? super E> predicate) {

    Precondition.param(predicate, "predicate").notNull();
    
    return new PredicateNotWrapper<E>(predicate);
  }

  /**
   * The method wraps the two {@link Predicate} objects as the left and right operands, and creates
   * a wrapper {@link Predicate} instance that returns the {@code &&} result of the test results of
   * the two operands.
   *
   * @param left the left operand, cannot be {@code null}.
   * @param right the right operand, cannot be {@code null}.
   * @param <E> the actual type of targets to test.
   * @return the wrapper created.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code left} or
   *         {@code right} is {@code null}.
   */
  public static <E> Predicate<E> and(Predicate<? super E> left, Predicate<? super E> right) {

    Precondition.param(left, "left").notNull();
    Precondition.param(right, "right").notNull();

    return new PredicateAndWrapper<E>(left, right);
  }

  /**
   * The method wraps the two {@link Predicate} objects as the left and right operands, and creates
   * a wrapper {@link Predicate} instance that returns the {@code ||} result of the test results of
   * the two operands.
   *
   * @param left the left operand, cannot be {@code null}.
   * @param right the right operand, cannot be {@code null}.
   * @param <E> the actual type of targets to test.
   * @return the wrapper created.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code left} or
   *         {@code right} is {@code null}.
   */
  public static <E> Predicate<E> or(Predicate<? super E> left, Predicate<? super E> right) {

    Precondition.param(left, "left").notNull();
    Precondition.param(right, "right").notNull();

    return new PredicateOrWrapper<E>(left, right);
  }

  /**
   * The method creates and returns a {@link PredicateBuilder} instance based on the given {@code base}.
   *
   * @param base the base {@link Predicate} of the builder to create, cannot be {@code null}.
   * @param <E> the actual target type to be tested by the predicate, hence the actual type signature
   *           of the builder.
   * @return the builder created.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code base} is {@code null}.
   */
  public static <E> PredicateBuilder<E> builder(Predicate<? super E> base) {
    
    Precondition.param(base, "base").notNull();

    return new PredicateBuilder<E>(Predicates.<E>dummyTrue()).and(base);
  }

  /*
   * Not to be instantiated or inherited.
   */
  private Predicates() { }
}


