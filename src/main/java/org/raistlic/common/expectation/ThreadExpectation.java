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

import org.raistlic.common.precondition.InvalidParameterException;
import org.raistlic.common.precondition.Precondition;

import java.util.function.Function;
import java.util.function.Predicate;

/**
 * @author Lei CHEN (2015-11-19)
 */
public class ThreadExpectation {

  private final Thread thread;

  private final Function<String, ? extends RuntimeException> exceptionProvider;

  public ThreadExpectation(Thread thread,
                           Function<String, ? extends RuntimeException> exceptionProvider) {

    if (thread == null) {
      throw new InvalidParameterException("'thread' cannot be null.");
    }
    if (exceptionProvider == null) {
      throw new InvalidParameterException("'exceptionProvider' cannot be null.");
    }
    this.thread = thread;
    this.exceptionProvider = exceptionProvider;
  }

  public void hasId(long id) {

    String message = "Current thread should have id " + id + ", but was " + thread.getId();
    hasId(id, message);
  }

  public void hasId(long id, String message) {

    if (thread.getId() != id) {
      throw exceptionProvider.apply(message);
    }
  }

  public void hasPriority(int priority) {

    String message = "Current thread '" + Thread.currentThread().getName() +
            "' should have priority of " + priority +
            ", but was " + thread.getPriority();
    hasPriority(priority, message);
  }

  public void hasPriority(int priority, String message) {

    if (thread.getPriority() != priority) {
      throw exceptionProvider.apply(message);
    }
  }

  public void isDaemon() {

    String message = "Current thread '" + thread.getName() +
            "' should be daemon thread, but was not.";
    isDaemon(message);
  }

  public void isDaemon(String message) {

    if (! thread.isDaemon()) {
      throw exceptionProvider.apply(message);
    }
  }

  public void isNotDaemon() {

    String message = "Current thread '" + Thread.currentThread().getName() +
            "' should NOT be daemon thread, but it was.";
    isNotDaemon(message);
  }

  public void isNotDaemon(String message) {

    if (thread.isDaemon()) {
      throw exceptionProvider.apply(message);
    }
  }

  public void isInterrupted() {

    String message = "Current thread '" + thread.getName() +
            "' is expected to be interrupted but not.";
    isInterrupted(message);
  }

  public void isInterrupted(String message) {

    if (! thread.isInterrupted()) {
      throw exceptionProvider.apply(message);
    }
  }

  public void isNotInterrupted() {

    String message = "Current thread '" + thread.getName() +
            "' is unexpectedly interrupted.";
    isNotInterrupted(message);
  }

  public void isNotInterrupted(String message) {

    if (thread.isInterrupted()) {
      throw exceptionProvider.apply(message);
    }
  }

  public void matches(Predicate<? super Thread> predicate) {

    String message = "Current thread does not match the specified predicate.";
    matches(predicate, message);
  }

  public void matches(Predicate<? super Thread> predicate, String message) {

    Precondition.param(predicate, "predicate").isNotNull();

    if (!predicate.test(Thread.currentThread())) {
      throw exceptionProvider.apply(message);
    }
  }
}
