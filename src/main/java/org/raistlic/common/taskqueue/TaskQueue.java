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

package org.raistlic.common.taskqueue;

import org.raistlic.common.precondition.InvalidContextException;
import org.raistlic.common.precondition.InvalidParameterException;
import org.raistlic.common.precondition.InvalidStateException;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * This interface defines the API of a single thread runnable queue implementation.
 *
 * @author Lei Chen (2013-12-19)
 * @since 1.0
 */
public interface TaskQueue {

  /**
   * The method submits a runnable task into the task queue to be executed.
   *
   * @param task the task to be scheduled, cannot be {@code null}.
   * @throws InvalidParameterException when {@code task} is {@code null}.
   * @throws InvalidStateException if the task queue is not running.
   */
  void schedule(Runnable task) throws InvalidParameterException, InvalidStateException;

  /**
   * The method submits a runnable task that has a returned result when executed into the queue,
   * and returns a {@link Promise} that references to the task.
   *
   * @param task the task to be scheduled, cannot be {@code null}.
   * @param <R> the actual return type of the {@link Task}'s run method.
   * @return the promise that references the scheduled task.
   * @throws InvalidParameterException when {@code task} is {@code null}.
   * @throws InvalidStateException if the task queue is not running.
   */
  <R> Promise<R> schedule(Task<R> task) throws InvalidParameterException, InvalidStateException;

  /**
   * The method submits the {@code task} into the task queue, waits until it's executed,
   * and returns the returned execution result.
   *
   * @param task the task to execute, cannot be {@code null}.
   * @param <R> the actual return type of the {@link Task}'s run method.
   * @return the {@code task} 's execution result.
   * @throws InvalidParameterException when {@code task} is {@code null}.
   * @throws InvalidStateException if the task queue is not running.
   * @throws InvalidContextException when the method is called within the task queue execution thread,
   *         see {@link #isTaskExecutionThread()} .
   * @throws TaskExecutionException if the {@code task}'s {@link java.util.concurrent.Callable#call()}
   *         method throws exception.
   * @throws java.lang.InterruptedException if the current calling thread is interrupted when waiting
   *         for the {@code task} to be executed.
   */
  <R> R scheduleAndWait(Task<R> task) throws InvalidParameterException,
          InvalidStateException,
          InvalidContextException,
          TaskExecutionException,
          InterruptedException;

  /**
   * The method submits the {@code task} into the task queue, waits up to {@code timeout} for it to
   * be executed, and returns the returned execution result.
   *
   * @param task the task to execute, cannot be {@code null}.
   * @param timeout the max amount of time that calling thread is going to wait for the task to be
   *        executed, cannot be negative.
   * @param timeUnit the unit of the {@code timeout} .
   * @param <R> the actual return type of the {@link Task}'s run method.
   * @return the {@code task} 's execution result.
   * @throws InvalidParameterException when {@code task} is {@code null}, or when {@code timeout} is
   *         less than {@code 0} .
   * @throws InvalidStateException if the task queue is not running.
   * @throws InvalidContextException when the method is called within the task queue execution thread,
   *         see {@link #isTaskExecutionThread()} .
   * @throws TaskExecutionException if the {@code task}'s {@link java.util.concurrent.Callable#call()}
   *         method throws exception.
   * @throws java.lang.InterruptedException if the current calling thread is interrupted when waiting
   *         for the {@code task} to be executed.
   * @throws java.util.concurrent.TimeoutException if the task is not executed within {@code timeout} .
   */
  <R> R scheduleAndWait(Task<R> task, long timeout, TimeUnit timeUnit)
          throws InvalidParameterException,
          InvalidStateException,
          InvalidContextException,
          TaskExecutionException,
          InterruptedException,
          TimeoutException;

  /**
   * The method returns whether the current (calling) thread is the task queue thread.
   *
   * @return {@code true} if the current (calling) thread is the task queue thread.
   * @throws InvalidStateException if the task queue is not running.
   */
  boolean isTaskExecutionThread() throws InvalidStateException;

  /**
   * The method returns whether the task queue is running. Attempting to submit a task to a not
   * running task queue will cause {@link IllegalStateException} .
   *
   * @return {@code true} if the task queue is running.
   */
  boolean isRunning();

  /**
   * The controller that holds the instance of the task queue, and controls its life cycle.
   */
  public static interface Controller {

    /**
     * The method returns the task queue that the handler is handling.
     *
     * @return the task queue that the handler is handling.
     */
    TaskQueue get();

    /**
     * Starts the task queue, if it is not already running; otherwise the method simply does nothing
     * and returns {@code false}.
     *
     * @return {@code true} if the task queue's running state is successfully changed as a result of
     *         the call.
     */
    boolean start();

    /**
     * Shuts down the task queue(if the queue is running), and the current (calling) thread waits
     * for the queue to shutdown, for {@code timeout} milliseconds.
     *
     * @param timeout the maximum number of milli-seconds to wait for the queue shutdown.
     * @param timeUnit the unit of the {@code timeout} .
     * @return {@code true} if the queue's running state is changed as a result of the call.
     * @throws java.lang.InterruptedException if the current (calling) thread is interrupted while
     *         waiting for the queue to shutdown.
     * @throws java.util.concurrent.TimeoutException if waiting time out, in which case the signal
     *         to shut down the queue is already sent, and that the queue may very well shut down
     *         at any time later after the exception is thrown.
     */
    boolean stop(long timeout, TimeUnit timeUnit) throws InterruptedException, TimeoutException;

    /**
     * The method signals the task queue to stop at an appropriate time.
     *
     * @param interruptCurrentTask {@code true} if the currently executed task should be interrupted.
     */
    void stop(boolean interruptCurrentTask);
  }
}
