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
package org.raistlic.common.predicate;

import org.raistlic.common.precondition.InvalidParameterException;
import org.raistlic.common.precondition.Precondition;

import java.util.function.Predicate;

/**
 * The class is a collection of static factory methods that creates and exports different types of 
 * {@link Predicate} instances.
 *
 * @author Lei CHEN
 * @since 1.3
 */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class Predicates {

  /**
   * The method returns an immutable singleton {@link Predicate} instance that always returns
   * {@code true} for all tests regardless of what the target is.
   *
   * @param <E> the actual type of targets to test.
   * @return the singleton {@link Predicate} instance.
   */
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
  public static <E> Predicate<E> dummyFalse() {
    
    return (Predicate<E>) DummyPredicate.FALSE;
  }

  /**
   * The method returns an immutable singleton {@link Predicate} instance that returns {@code true}
   * only for {@code null} values in its test method.
   *
   * @param <E> the actual type signature to be exported.
   * @return an immutable, singleton {@link Predicate} that returns {@code true} only for
   *         {@code null} values in its test method.
   */
  public static <E> Predicate<E> isNull() {

    return ObjectIsNullPredicate.INSTANCE;
  }

  /**
   * The method returns an immutable singleton {@link Predicate} instance that returns {@code true}
   * only for not {@code null} values in its test method.
   *
   * @param <E> the actual type signature to be exported.
   * @return an immutable, singleton {@link Predicate} that returns {@code true} only for not
   *         {@code null} values in its test method.
   */
  public static <E> Predicate<E> notNull() {

    return ObjectIsNotNullPredicate.INSTANCE;
  }

  /**
   * The method returns a {@link Predicate} instance to test whether an object is of the
   * specified {@code type} .
   *
   * @param type the type used to test objects, cannot be {@code null}.
   * @return the predicate instance created.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code type} is {@code null}.
   */
  public static Predicate<Object> instanceOf(Class<?> type) {

    Precondition.assertParam(type != null, "Predicates.instanceOf(type): type cannot be null.");
    return new ObjectIsInstanceOfTypePredicate(type);
  }

  /**
   * The method returns a {@link Predicate} instance that tests whether an instance of type {@code <E>}
   * is equal to the specified {@code reference}; when the {@code reference} is {@code null}, the
   * exported {@link Predicate} is functionally the same as the one exported by {@link #isNull()} .
   *
   * @param reference the reference used to create the {@link Predicate} , and to test candidates.
   * @param <E> the actual type of the {@code reference} and candidates to be tested.
   * @return the {@link Predicate} created.
   */
  public static <E> Predicate<E> equalTo(E reference) {

    return new EqualsPredicate<E>(reference);
  }

  /**
   * The method returns a {@link Predicate} instance that tests whether an instance of type {@code <E>}
   * is NOT equal to the specified {@code reference}; when the {@code reference} is {@code null}, the
   * exported {@link Predicate} is functionally the same as the one exported by {@link #notNull()} .
   *
   * @param reference the reference used to create the {@link Predicate} , and to test candidates.
   * @param <E> the actual type of the {@code reference} and candidates to be tested.
   * @return the {@link Predicate} created.
   */
  public static <E> Predicate<E> notEqualTo(E reference) {

    return not(equalTo(reference));
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

    Precondition.assertParam(predicate != null, "Predicates.not(predicate): predicate cannot be null.");
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

    Precondition.assertParam(left != null, "Predicates.and(left, right): left cannot be null.");
    Precondition.assertParam(right != null, "Predicates.and(left, right): right cannot be null.");
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

    Precondition.assertParam(left != null, "Predicates.or(left, right): left cannot be null.");
    Precondition.assertParam(right != null, "Predicates.or(left, right): right cannot be null.");
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
    
    Precondition.assertParam(base != null, "Predicates.builder(base): base cannot be null.");
    return new PredicateBuilder<E>(Predicates.<E>dummyTrue()).and(base);
  }

  private static class EqualsPredicate<E> implements Predicate<E> {

    private E object;

    private EqualsPredicate(E object) {

      this.object = object;
    }

    @Override
    public boolean test(E o) {

      if (object == null) {
        return o == null;
      }
      else {
        return object.equals(o);
      }
    }
  }

  private enum ObjectIsNullPredicate implements Predicate {

    INSTANCE;

    @Override
    public boolean test(Object o) {

      return o == null;
    }
  }

  private enum ObjectIsNotNullPredicate implements Predicate {

    INSTANCE;

    @Override
    public boolean test(Object o) {

      return o != null;
    }
  }

  private static class ObjectIsInstanceOfTypePredicate implements Predicate<Object> {

    private final Class<?> type;

    private ObjectIsInstanceOfTypePredicate(Class<?> type) {

      this.type = type;
    }

    @Override
    public boolean test(Object o) {

      return (o != null) && type.isInstance(o);
    }
  }

  /*
   * Not to be instantiated or inherited.
   */
  private Predicates() { }
}


