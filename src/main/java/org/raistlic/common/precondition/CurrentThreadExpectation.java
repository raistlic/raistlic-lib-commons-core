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

import javax.swing.*;
import java.util.function.Function;

/**
 * @author Lei CHEN (2015-11-19)
 */
public class CurrentThreadExpectation {

  private final Function<String, ? extends RuntimeException> exceptionProvider;

  public CurrentThreadExpectation(Function<String, ? extends RuntimeException> exceptionProvider) {

    if (exceptionProvider == null) {
      throw new InvalidParameterException("'exceptionProvider' cannot be null.");
    }
    this.exceptionProvider = exceptionProvider;
  }

  public void hasId(long id) {

    String message = "Current thread should have id " + id + ", but was " + Thread.currentThread().getId();
    hasId(id, message);
  }

  public void hasId(long id, String message) {

    if (Thread.currentThread().getId() != id) {
      throw exceptionProvider.apply(message);
    }
  }

  public void hasPriority(int priority) {

    String message = "Current thread '" + Thread.currentThread().getName() +
            "' should have priority of " + priority +
            ", but was " + Thread.currentThread().getPriority();
    hasPriority(priority, message);
  }

  public void hasPriority(int priority, String message) {

    if (Thread.currentThread().getPriority() != priority) {
      throw exceptionProvider.apply(message);
    }
  }

  public void isDaemon() {

    String message = "Current thread '" + Thread.currentThread().getName() +
            "' should be daemon thread, but was not.";
    isDaemon(message);
  }

  public void isDaemon(String message) {

    if (! Thread.currentThread().isDaemon()) {
      throw exceptionProvider.apply(message);
    }
  }

  public void isNotDaemon() {

    String message = "Current thread '" + Thread.currentThread().getName() +
            "' should NOT be daemon thread, but it was.";
    isNotDaemon(message);
  }

  public void isNotDaemon(String message) {

    if (Thread.currentThread().isDaemon()) {
      throw exceptionProvider.apply(message);
    }
  }

  public void isInterrupted() {

    String message = "Current thread '" + Thread.currentThread().getName() +
            "' is expected to be interrupted but not.";
    isInterrupted(message);
  }

  public void isInterrupted(String message) {

    if (! Thread.currentThread().isInterrupted()) {
      throw exceptionProvider.apply(message);
    }
  }

  public void isNotInterrupted() {

    String message = "Current thread '" + Thread.currentThread().getName() +
            "' is unexpectedly interrupted.";
    isNotInterrupted(message);
  }

  public void isNotInterrupted(String message) {

    if (Thread.currentThread().isInterrupted()) {
      throw exceptionProvider.apply(message);
    }
  }

  public void isEventDispatchingThread() {

    String message = "Current thread is expected to be EDT, but was not: " +
            Thread.currentThread().getName();
    isEventDispatchingThread(message);
  }

  public void isEventDispatchingThread(String message) {

    if (! SwingUtilities.isEventDispatchThread()) {
      throw exceptionProvider.apply(message);
    }
  }

  public void isNotEventDispatchingThread() {

    String message = "Current thread should not be EDT.";
    isNotEventDispatchingThread(message);
  }

  public void isNotEventDispatchingThread(String message) {

    if (SwingUtilities.isEventDispatchThread()) {
      throw exceptionProvider.apply(message);
    }
  }
}
