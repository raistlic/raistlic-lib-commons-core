/*
 * Copyright 2016 Lei CHEN (raistlic@gmail.com)
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

package org.raistlic.common.assertion;

import org.raistlic.common.precondition.Precondition;

import java.util.function.Function;
import java.util.function.Predicate;

/**
 * Default implementation for {@link ThreadAssertion} .
 */
final class ThreadAssertionDefault implements ThreadAssertion {

  private Thread candidate;

  private final Function<String, ? extends RuntimeException> exceptionMapper;

  ThreadAssertionDefault(Thread candidate,
                         Function<String, ? extends RuntimeException> exceptionMapper) {

    this.exceptionMapper = exceptionMapper;
    setCandidate(candidate);
  }

  final void setCandidate(Thread candidate) {

    Precondition.assertParam(candidate != null, "'candidate' should not be null, but it is.");
    this.candidate = candidate;
  }

  @Override
  public ThreadAssertion hasId(long id) {

    if (!threadHasId(id)) {
      String message = "Current thread should have id " + id + ", but it's id is " + candidate.getId();
      throw exceptionMapper.apply(message);
    }
    return this;
  }

  @Override
  public ThreadAssertion hasId(long id, String message) {

    if (!threadHasId(id)) {
      throw exceptionMapper.apply(message);
    }
    return this;
  }

  private boolean threadHasId(long id) {

    return candidate.getId() == id;
  }

  @Override
  public ThreadAssertion hasPriority(int priority) {

    if (!threadHasPriority(priority)) {
      String message = "Current thread '" + Thread.currentThread().getName() +
        "' should have priority of " + priority +
        ", but it's priority is " + candidate.getPriority();
      throw exceptionMapper.apply(message);
    }
    return this;
  }

  @Override
  public ThreadAssertion hasPriority(int priority, String message) {

    if (!threadHasPriority(priority)) {
      throw exceptionMapper.apply(message);
    }
    return this;
  }

  private boolean threadHasPriority(int priority) {

    return candidate.getPriority() == priority;
  }

  @Override
  public ThreadAssertion isDaemon() {

    if (!candidate.isDaemon()) {
      String message = "Current thread '" + candidate.getName() +
        "' should be daemon thread, but it is not.";
      throw exceptionMapper.apply(message);
    }
    return this;
  }

  @Override
  public ThreadAssertion isDaemon(String message) {

    if (!candidate.isDaemon()) {
      throw exceptionMapper.apply(message);
    }
    return this;
  }

  @Override
  public ThreadAssertion isNotDaemon() {

    if (candidate.isDaemon()) {
      String message = "Current thread '" + Thread.currentThread().getName() +
        "' should NOT be daemon thread, but it is.";
      throw exceptionMapper.apply(message);
    }
    return this;
  }

  @Override
  public ThreadAssertion isNotDaemon(String message) {

    if (candidate.isDaemon()) {
      throw exceptionMapper.apply(message);
    }
    return this;
  }

  @Override
  public ThreadAssertion isInterrupted() {

    if (!candidate.isInterrupted()) {
      String message = "Current thread '" + candidate.getName() +
        "' is expected to be interrupted but not.";
      throw exceptionMapper.apply(message);
    }
    return this;
  }

  @Override
  public ThreadAssertion isInterrupted(String message) {

    if (!candidate.isInterrupted()) {
      throw exceptionMapper.apply(message);
    }
    return this;
  }

  @Override
  public ThreadAssertion isNotInterrupted() {

    if (candidate.isInterrupted()) {
      String message = "Current thread '" + candidate.getName() +
        "' is unexpectedly interrupted.";
      throw exceptionMapper.apply(message);
    }
    return this;
  }

  @Override
  public ThreadAssertion isNotInterrupted(String message) {

    if (candidate.isInterrupted()) {
      throw exceptionMapper.apply(message);
    }
    return this;
  }

  @Override
  public ThreadAssertion matches(Predicate<? super Thread> predicate) {

    Precondition.assertParam(predicate != null, "'predicate' should not be null, but it is.");

    if (!predicate.test(candidate)) {
      String message = "Current thread does not match the specified predicate '" + predicate + "'.";
      throw exceptionMapper.apply(message);
    }
    return this;
  }

  @Override
  public ThreadAssertion matches(Predicate<? super Thread> predicate, String message) {

    Precondition.assertParam(predicate != null, "'predicate' should not be null, but it is.");

    if (!predicate.test(candidate)) {
      throw exceptionMapper.apply(message);
    }
    return this;
  }
}
