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
import org.raistlic.common.precondition.Precondition;
import org.raistlic.common.predicate.Predicates;
import org.raistlic.common.util.ExceptionHandler;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Predicate;

/**
 * @author Lei Chen (2015-11-24)
 */
final class DefaultTaskQueue implements TaskQueue, TaskQueue.Controller {

  private final ExecutorService executorService;

  private final QueueRunnable queueRunnable;

  private final ExceptionHandler exceptionHandler;

  private final AtomicBoolean running;

  private final Predicate<? super Thread> isNotTaskQueuePredicate;

  private volatile Thread taskQueueThread;

  private volatile LinkedBlockingQueue<Runnable> queue;

  DefaultTaskQueue(ThreadFactory threadFactory,
                   ExceptionHandler exceptionHandler) {

    Precondition.param(threadFactory, "threadFactory").notNull();
    Precondition.param(exceptionHandler, "exceptionHandler").notNull();

    this.executorService = Executors.newSingleThreadExecutor(threadFactory);
    this.exceptionHandler = exceptionHandler;
    this.queueRunnable = this.new QueueRunnable();
    this.running = new AtomicBoolean(false);
    this.isNotTaskQueuePredicate = Predicates.not(this.new TaskQueueThreadPredicate());
  }

  @Override
  public TaskQueue get() {

    return this;
  }

  @Override
  public boolean start() {

    if (running.getAndSet(true)) {
      return false;
    }
    else {
      queue = new LinkedBlockingQueue<Runnable>();
      executorService.submit(queueRunnable);
      return true;
    }
  }

  @Override
  public boolean stop(long timeout, TimeUnit timeUnit) throws InterruptedException, TimeoutException {

    if (running.getAndSet(false)) {
      executorService.shutdownNow();
      queue.offer(EmptyRunnable.INSTANCE);
      executorService.awaitTermination(timeout, timeUnit);
      return true;
    }
    else {
      return false;
    }
  }

  @Override
  public void stop(boolean interruptCurrentTask) {

    if (!running.getAndSet(false)) {
      return;
    }
    if (interruptCurrentTask) {
      executorService.shutdownNow();
    }
    else {
      executorService.shutdown();
    }
    queue.offer(EmptyRunnable.INSTANCE);
  }

  @Override
  public void schedule(Runnable task) throws InvalidParameterException, InvalidStateException {

    Precondition.param(task, "task").notNull();
    Precondition.state(running.get(), "running").isTrue();

    queue.offer(this.new ExceptionFreeTaskWrapper(task));
  }

  @Override
  public <R> Promise<R> schedule(Task<R> task) throws InvalidParameterException, InvalidStateException {

    Precondition.param(task, "task").notNull();

    DefaultPromise<R> defaultPromise = new DefaultPromise<R>(task, exceptionHandler);
    queue.offer(defaultPromise);
    return defaultPromise;
  }

  @Override
  public <R> R scheduleAndWait(Task<R> task)
          throws InvalidParameterException,
                 InvalidStateException,
                 InvalidContextException,
                 TaskExecutionException,
                 InterruptedException {

    Precondition.param(task, "task").notNull();
    Precondition.threadContext().matches(
            isNotTaskQueuePredicate,
            "The method cannot be invoked with in the task queue execution thread."
    );
    Precondition.state(running.get(), "running").isTrue();

    Promise<R> promise = schedule(task);
    return promise.get();
  }

  @Override
  public <R> R scheduleAndWait(Task<R> task, long timeout, TimeUnit timeUnit)
          throws InvalidParameterException,
                 InvalidStateException,
                 InvalidContextException,
                 TaskExecutionException,
                 InterruptedException,
                 TimeoutException {

    Precondition.param(timeout, "timeout").greaterThanOrEqualTo(0L);
    Precondition.param(timeUnit, "timeUnit").notNull();
    Precondition.param(task, "task").notNull();
    Precondition.threadContext().matches(
            isNotTaskQueuePredicate,
            "The method cannot be invoked with in the task queue execution thread."
    );
    Precondition.state(running.get(), "running").isTrue();

    Promise<R> promise = schedule(task);
    return promise.get(timeout, timeUnit);
  }

  @Override
  public boolean isTaskExecutionThread() throws InvalidStateException {

    return Thread.currentThread() == taskQueueThread;
  }

  @Override
  public boolean isRunning() {

    return running.get();
  }

  private final class QueueRunnable implements Runnable {

    @Override
    public void run() {

      taskQueueThread = Thread.currentThread();
      while (running.get()) {
        Runnable runnable;
        try {
          runnable = queue.take();
        }
        catch (InterruptedException ex) {
          break;
        }
        if (running.get()) {
          runnable.run();
        }
      }
      running.set(false);
      taskQueueThread = null;
    }
  }

  private final class ExceptionFreeTaskWrapper implements Runnable {

    private final Runnable runnable;

    private ExceptionFreeTaskWrapper(Runnable runnable) {

      this.runnable = runnable;
    }

    @Override
    public void run() {

      try {
        runnable.run();
      }
      catch (Exception ex) {
        exceptionHandler.exceptionOccur(Thread.currentThread(), ex);
      }
    }
  }

  private final class TaskQueueThreadPredicate implements Predicate<Thread> {

    @Override
    public boolean test(Thread thread) {

      return thread == taskQueueThread;
    }
  }

  private enum EmptyRunnable implements Runnable {

    INSTANCE;

    @Override
    public void run() {
      // do nothing
    }
  }
}
