package org.raistlic.common.expectation;

import java.util.function.Predicate;

public interface ThreadExpectation {

  ThreadExpectation hasId(long id);

  ThreadExpectation hasId(long id, String message);

  ThreadExpectation hasPriority(int priority);

  ThreadExpectation hasPriority(int priority, String message);

  ThreadExpectation isDaemon();

  ThreadExpectation isDaemon(String message);

  ThreadExpectation isNotDaemon();

  ThreadExpectation isNotDaemon(String message);

  ThreadExpectation isInterrupted();

  ThreadExpectation isInterrupted(String message);

  ThreadExpectation isNotInterrupted();

  ThreadExpectation isNotInterrupted(String message);

  ThreadExpectation matches(Predicate<? super Thread> predicate);

  ThreadExpectation matches(Predicate<? super Thread> predicate, String message);
}
