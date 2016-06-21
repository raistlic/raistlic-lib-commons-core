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

import org.raistlic.common.precondition.Precondition;
import org.raistlic.common.util.ExceptionHandler;

import java.util.concurrent.ThreadFactory;

/**
 * @author Lei Chen (2015-11-24)
 */
public final class TaskQueues {

  public static TaskQueueBuilder builder() {

    return new DefaultTaskQueueBuilder();
  }

  private static final class DefaultTaskQueueBuilder implements TaskQueueBuilder {

    private Boolean daemon;

    private Integer priority;

    private String name;

    private ExceptionHandler exceptionHandler = DefaultTaskQueueExceptionHandler.INSTANCE;

    @Override
    public TaskQueueBuilder withThreadAsDaemon(boolean daemon) {

      this.daemon = daemon;
      return this;
    }

    @Override
    public TaskQueueBuilder withThreadPriority(int priority) {

      this.priority = priority;
      return this;
    }

    @Override
    public TaskQueueBuilder withThreadName(String name) {

      this.name = name;
      return this;
    }

    @Override
    public TaskQueueBuilder withExceptionHandler(ExceptionHandler exceptionHandler) {

      this.exceptionHandler = exceptionHandler;
      return this;
    }

    @Override
    public TaskQueue.Controller get() {

      TaskQueueThreadFactory taskQueueThreadFactory = new TaskQueueThreadFactory(
              this.daemon, this.priority, this.name, this.exceptionHandler
      );
      return new DefaultTaskQueue(taskQueueThreadFactory, exceptionHandler);
    }
  }

  private static final class TaskQueueThreadFactory implements ThreadFactory {

    private final Boolean daemon;

    private final Integer priority;

    private final String name;

    private final ExceptionHandler exceptionHandler;

    private TaskQueueThreadFactory(Boolean daemon,
                                   Integer priority,
                                   String name,
                                   ExceptionHandler exceptionHandler) {

      this.daemon = daemon;
      this.priority = priority;
      this.name = name;
      this.exceptionHandler = (exceptionHandler == null) ?
              DefaultTaskQueueExceptionHandler.INSTANCE : exceptionHandler;
    }

    @Override
    public Thread newThread(Runnable r) {

      Precondition.param(r).isNotNull();

      Thread thread = new Thread(r);
      if (daemon != null) {
        thread.setDaemon(daemon);
      }
      if (priority != null) {
        thread.setPriority(priority);
      }
      if (name != null) {
        thread.setName(name);
      }
      thread.setUncaughtExceptionHandler(new UncaughtExceptionHandlerAdapter(exceptionHandler));
      return thread;
    }
  }

  private static final class UncaughtExceptionHandlerAdapter implements Thread.UncaughtExceptionHandler {

    private final ExceptionHandler exceptionHandler;

    private UncaughtExceptionHandlerAdapter(ExceptionHandler exceptionHandler) {

      assert exceptionHandler != null;

      this.exceptionHandler = exceptionHandler;
    }

    @Override
    public void uncaughtException(Thread t, Throwable e) {

      exceptionHandler.exceptionOccur(t, e);
    }
  }

  private TaskQueues() { }
}
