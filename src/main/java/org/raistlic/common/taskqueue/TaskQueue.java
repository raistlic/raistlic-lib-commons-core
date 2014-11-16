/*
 * Copyright 2013 Lei CHEN (raistlic@gmail.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.raistlic.common.taskqueue;

import java.util.concurrent.Callable;
import java.util.concurrent.TimeoutException;

/**
 * This interface defines the API of a single thread runnable queue implementation.
 *
 * @author Lei CHEN (2013-12-19)
 * @since 1.0
 */
public interface TaskQueue {

  /**
   * The method submits a runnable task into the task queue to be executed.
   *
   * @param task the task to submit, cannot be {@code null}.
   *
   * @throws java.lang.NullPointerException if {@code task} is {@code null}.
   * @throws java.lang.IllegalStateException if the task queue is not running.
   */
  void invokeLater(Runnable task) throws NullPointerException, IllegalStateException;

  /**
   * The method submits the {@code task} into the task queue, waits until it's executed,
   * and returns the returned execution result.
   *
   * @param task the task to execute, cannot be {@code null}.
   * @param <R> the referenced type of the task result.
   * @return the callable {@code task} execution result.
   *
   * @throws java.lang.NullPointerException if {@code task} is {@code null}.
   * @throws java.lang.IllegalStateException if the task queue is not running.
   * @throws TaskExecutionException if the {@code task}'s {@link java.util.concurrent.Callable#call()}
   *         method throws exception, or if the current calling thread is interrupted when waiting
   *         for the {@code task} to be executed.
   */
  <R> R invokeAndWait(Callable<R> task) throws NullPointerException, IllegalStateException,
          TaskExecutionException;

  /**
   * The method returns whether the current (calling) thread is the task queue thread.
   *
   * @return {@code true} if the current (calling) thread is the task queue thread.
   */
  boolean isTaskExecutionThread() throws IllegalStateException;

  /**
   * The method returns whether the task queue is running. Attempting to submit a task to a not
   * running task queue will cause {@link IllegalStateException} .
   *
   * @return {@code true} if the task queue is running.
   */
  boolean isRunning();

  /**
   * The handler that holds the instance of the task queue, and controls its life cycle.
   */
  public static interface Handle {

    /**
     * The method returns the task queue that the handler is handling.
     *
     * @return the task queue that the handler is handling.
     */
    TaskQueue get();

    /**
     * Starts the task queue, if it is not already running.
     *
     * @return {@code true} if the task queue's running state is changed as a result of the call.
     */
    boolean start();

    /**
     * Shuts down the task queue(if the queue is running), and the current (calling) thread waits
     * for the queue to shutdown, for {@code timeout} milliseconds.
     *
     * @param timeout the maximum number of milli-seconds to wait for the queue shutdown.
     * @return {@code true} if the queue's running state is changed as a result of the call.
     *
     * @throws java.lang.InterruptedException if the current (calling) thread is interrupted while
     *         waiting for the queue to shutdown.
     * @throws java.util.concurrent.TimeoutException if waiting time out, in which case the signal
     *         to shut down the queue is already sent, and that the queue may very well shut down
     *         at any time later after the exception is thrown.
     */
    boolean shutdown(long timeout) throws InterruptedException, TimeoutException;

    /**
     * The method signals the task queue to shutdown, when the currently executing task is done.
     */
    void shutdownLater();
  }
}
