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
import org.raistlic.common.precondition.Precondition;
import org.raistlic.common.util.ExceptionHandler;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

/**
 * @author Lei Chen (2015-11-29)
 */
final class DefaultPromise<R> implements Promise<R>, Runnable {

  private volatile R result;

  private volatile TaskExecutionException exception;

  private final AtomicReference<Consumer<? super R>> resultConsumer;

  private final AtomicReference<ExceptionHandler> exceptionHandler;

  private final Task<R> task;

  private final ExceptionHandler taskExceptionHandler;

  private final CountDownLatch cd;

  private final AtomicBoolean canceled;

  private final AtomicBoolean done;

  DefaultPromise(Task<R> task, ExceptionHandler taskExceptionHandler) {

    Precondition.param(task).isNotNull();
    Precondition.param(taskExceptionHandler).isNotNull();

    this.resultConsumer = new AtomicReference<Consumer<? super R>>(null);
    this.exceptionHandler = new AtomicReference<ExceptionHandler>(null);

    this.task = task;
    this.taskExceptionHandler = taskExceptionHandler;
    this.canceled = new AtomicBoolean(false);
    this.done = new AtomicBoolean(false);
    this.cd = new CountDownLatch(1);
  }

  private void onErrorCallback() {

    ExceptionHandler onErrorHandler = exceptionHandler.getAndSet(null);
    TaskExecutionException error = exception;
    if (error != null && onErrorHandler != null) {
      onErrorHandler.exceptionOccur(Thread.currentThread(), exception);
    }
  }

  private void onResultCallback() {

    Consumer<? super R> consumer = this.resultConsumer.getAndSet(null);
    TaskExecutionException error = exception;
    R r = this.result;
    if (error == null && consumer != null) {
      consumer.accept(r);
    }
  }

  @Override
  public boolean cancel(boolean mayInterruptIfRunning) {

    return (!done.get()) && (!canceled.getAndSet(true));
  }

  @Override
  public boolean isCancelled() {

    return canceled.get();
  }

  @Override
  public boolean isDone() {

    return done.get();
  }

  @Override
  public R get() throws InterruptedException, TaskExecutionException, InvalidContextException {

    cd.await();
    TaskExecutionException tee = exception;
    if (tee != null) {
      throw tee;
    }
    return result;
  }

  @Override
  public R get(long timeout, TimeUnit timeUnit) throws InterruptedException, TimeoutException, TaskExecutionException, InvalidContextException {

    if (!cd.await(timeout, timeUnit)) {
      throw new TimeoutException();
    }
    TaskExecutionException tee = exception;
    if (tee != null) {
      throw tee;
    }
    return result;
  }

  @Override
  public Promise<R> onResult(Consumer<? super R> resultConsumer) {

    Precondition.param(resultConsumer).isNotNull();

    this.resultConsumer.set(resultConsumer);
    if (done.get()) {
      this.onResultCallback();
    }
    return this;
  }

  @Override
  public Promise<R> onError(ExceptionHandler exceptionHandler) {

    Precondition.param(exceptionHandler).isNotNull();

    this.exceptionHandler.set(exceptionHandler);
    if (done.get()) {
      this.onErrorCallback();
    }
    return this;
  }

  @Override
  public void run() {

    synchronized (done) {

      Precondition.context(done.get()).isFalse();

      if (canceled.get()) {
        return;
      }

      try {
        result = task.run();
      }
      catch (Exception ex) {
        if (ex instanceof TaskExecutionException) {
          exception = (TaskExecutionException) ex;
        }
        else {
          exception = new TaskExecutionException(ex);
        }
        taskExceptionHandler.exceptionOccur(Thread.currentThread(), exception);
      }

      try {
        this.onResultCallback();
        this.onErrorCallback();
      }
      catch (Exception ex) {
        taskExceptionHandler.exceptionOccur(Thread.currentThread(), ex);
      }
      finally {
        done.set(true);
        cd.countDown();
      }
    }
  }
}
