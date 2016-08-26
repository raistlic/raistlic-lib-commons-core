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

package org.raistlic.common.expectation;

import org.raistlic.common.precondition.Precondition;

import java.util.function.Function;
import java.util.function.Predicate;

/**
 * @author Lei CHEN (2015-11-19)
 */
final class ThreadExpectationDefault implements ThreadExpectation {

  private final Thread thread;

  private final Function<String, ? extends RuntimeException> exceptionMapper;

  ThreadExpectationDefault(Thread thread,
                           Function<String, ? extends RuntimeException> exceptionMapper) {

    Precondition.assertParam(thread != null, "'thread' should not be null, but it is.");
    Precondition.assertParam(exceptionMapper != null, "'exceptionMapper' should not be null, but it is.");

    this.thread = thread;
    this.exceptionMapper = exceptionMapper;
  }

  @Override
  public ThreadExpectation hasId(long id) {

    if (!threadHasId(id)) {
      String message = "Current thread should have id " + id + ", but it's id is " + thread.getId();
      throw exceptionMapper.apply(message);
    }
    return this;
  }

  @Override
  public ThreadExpectation hasId(long id, String message) {

    if (!threadHasId(id)) {
      throw exceptionMapper.apply(message);
    }
    return this;
  }

  private boolean threadHasId(long id) {

    return thread.getId() == id;
  }

  @Override
  public ThreadExpectation hasPriority(int priority) {

    if (!threadHasPriority(priority)) {
      String message = "Current thread '" + Thread.currentThread().getName() +
              "' should have priority of " + priority +
              ", but it's priority is " + thread.getPriority();
      throw exceptionMapper.apply(message);
    }
    return this;
  }

  @Override
  public ThreadExpectation hasPriority(int priority, String message) {

    if (!threadHasPriority(priority)) {
      throw exceptionMapper.apply(message);
    }
    return this;
  }

  private boolean threadHasPriority(int priority) {

    return thread.getPriority() == priority;
  }

  @Override
  public ThreadExpectation isDaemon() {

    if (!thread.isDaemon()) {
      String message = "Current thread '" + thread.getName() +
              "' should be daemon thread, but it is not.";
      throw exceptionMapper.apply(message);
    }
    return this;
  }

  @Override
  public ThreadExpectation isDaemon(String message) {

    if (!thread.isDaemon()) {
      throw exceptionMapper.apply(message);
    }
    return this;
  }

  @Override
  public ThreadExpectation isNotDaemon() {

    if (thread.isDaemon()) {
      String message = "Current thread '" + Thread.currentThread().getName() +
              "' should NOT be daemon thread, but it is.";
      throw exceptionMapper.apply(message);
    }
    return this;
  }

  @Override
  public ThreadExpectation isNotDaemon(String message) {

    if (thread.isDaemon()) {
      throw exceptionMapper.apply(message);
    }
    return this;
  }

  @Override
  public ThreadExpectation isInterrupted() {

    if (!thread.isInterrupted()) {
      String message = "Current thread '" + thread.getName() +
              "' is expected to be interrupted but not.";
      throw exceptionMapper.apply(message);
    }
    return this;
  }

  @Override
  public ThreadExpectation isInterrupted(String message) {

    if (!thread.isInterrupted()) {
      throw exceptionMapper.apply(message);
    }
    return this;
  }

  @Override
  public ThreadExpectation isNotInterrupted() {

    if (thread.isInterrupted()) {
      String message = "Current thread '" + thread.getName() +
              "' is unexpectedly interrupted.";
      throw exceptionMapper.apply(message);
    }
    return this;
  }

  @Override
  public ThreadExpectation isNotInterrupted(String message) {

    if (thread.isInterrupted()) {
      throw exceptionMapper.apply(message);
    }
    return this;
  }

  @Override
  public ThreadExpectation matches(Predicate<? super Thread> predicate) {

    Precondition.assertParam(predicate != null, "'predicate' should not be null, but it is.");

    if (!predicate.test(thread)) {
      String message = "Current thread does not match the specified predicate '" + predicate + "'.";
      throw exceptionMapper.apply(message);
    }
    return this;
  }

  @Override
  public ThreadExpectation matches(Predicate<? super Thread> predicate, String message) {

    Precondition.assertParam(predicate != null, "'predicate' should not be null, but it is.");

    if (!predicate.test(thread)) {
      throw exceptionMapper.apply(message);
    }
    return this;
  }
}
