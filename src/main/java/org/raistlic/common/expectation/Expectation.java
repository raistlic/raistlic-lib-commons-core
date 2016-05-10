package org.raistlic.common.expectation;

import java.util.function.Predicate;

public interface Expectation<C, E extends Expectation<C, E>> {

  /**
   * The method claims that the {@code candidate} should be {@code null}, otherwise a runtime
   * exception will be thrown.
   *
   * @return the expectation instance itself, for method calling chain.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  E isNull();

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
  E isNull(String message);

  /**
   * The method claims that the {@code candidate} should not be {@code null}, otherwise a runtime
   * exception will be thrown.
   *
   * @return the expectation instance itself, for method calling chain.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  E isNotNull();

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
  E isNotNull(String message);

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
  E isEqualTo(C target);

  /**
   * The method claims that the {@code candidate} should be equal to the {@code target}, otherwise a
   * runtime exception with the specified {@code message} will be thrown.
   *
   * @param target  the reference target which the candidate should be equal to, or {@code null} if
   *                the candidate should be {@code null}.
   * @param message the message to be thrown with the exception, in case the check fails.
   * @return the expectation instance itself, for method calling chain.
   *
   * @throws java.lang.RuntimeException the exception of a specific type, depending on the context
   *         where the expectation is used.
   */
  E isEqualTo(C target, String message);

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
  E isNotEqualTo(C target);

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
  E isNotEqualTo(C target, String message);

  /**
   * The method claims that the {@code candidate} should be an instance of the specified {@code type} ,
   * otherwise a runtime exception will be thrown.
   *
   * @param type the type of which the {@code candidate} claims to be.
   * @return the expectation instance itself, for method calling chain.
   */
  E isInstanceOf(Class<?> type);

  /**
   * The method claims that the {@code candidate} should be an instance of the specified {@code type} ,
   * otherwise a runtime exception with the specified {@code message} will be thrown.
   *
   * @param type the type of which the {@code candidate} claims to be.
   * @param message the message to be thrown with the exception, in case the check fails.
   * @return the expectation instance itself, for method calling chain.
   */
  E isInstanceOf(Class<?> type, String message);

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
  E matches(Predicate<? super C> predicate);

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
  E matches(Predicate<? super C> predicate, String message);
}
