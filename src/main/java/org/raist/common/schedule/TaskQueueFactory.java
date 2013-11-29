/*
 * Copyright 2013 Lei CHEN (raistlic@gmail.com)
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

package org.raist.common.schedule;

import java.util.concurrent.ExecutorService;

/**
 *
 * @author Lei CHEN
 */
public class TaskQueueFactory {
  
  public static TaskQueue createTaskQueue(ExecutorService executor) {

    throw new UnsupportedOperationException("not implemented yet");
//
//    if( executor == null )
//      throw new NullPointerException("executor is null.");
//
//    return new DefaultTaskQueueHandle(executor);
  }
//
//  private static class DefaultTaskQueueHandle implements Handle<TaskQueue> {
//
//    private ExecutorService executor;
//    private DefaultTaskQueue taskQueue;
//
//    private final AtomicBoolean on;
//
//    private DefaultTaskQueueHandle(ExecutorService executor) {
//
//      this.executor = executor;
//      this.taskQueue = new DefaultTaskQueue();
//
//      this.on = new AtomicBoolean(false);
//    }
//
//    @Override
//    public TaskQueue get() {
//
//      return taskQueue;
//    }
//
//    @Override
//    public void switchOn() {
//
//      if( !on.getAndSet(true) )
//        executor.execute(taskQueue);
//    }
//
//    @Override
//    public void switchOff() {
//
//      taskQueue.stop();
//    }
//  }
//
//  private static class DefaultTaskQueue implements TaskQueue, Runnable {
//
//    private final LinkedBlockingDeque<Runnable> queue =
//            new LinkedBlockingDeque<Runnable>();
//
//    private volatile UncaughtExceptionHandler handler;
//
//    private volatile boolean running;
//
//    @Override
//    public void schedule(Runnable runnable) {
//
//      if( runnable == null )
//        throw new NullPointerException("Runnable is null.");
//
//      queue.addLast(new RunnableAdapter(this, runnable));
//    }
//
//    @Override
//    public <E> E scheduleAndWait(Callable<E> callable) throws Exception {
//
//      CallableAdapter<E> adapter = new CallableAdapter<E>(this, callable);
//      queue.addLast(adapter);
//      return adapter.get();
//    }
//
//    @Override
//    public void setExceptionHandler(UncaughtExceptionHandler handler) {
//
//      this.handler = handler;
//    }
//
//    @Override
//    public void run() {
//
//      running = true;
//
//      while (running)
//        queue.removeFirst().run();
//    }
//
//    void stop() {
//
//      running = false;
//      schedule(new Runnable() {
//
//        @Override
//        public void run() {}
//      });
//    }
//  }
//
//  private static class RunnableAdapter implements Runnable {
//
//    private final DefaultTaskQueue parent;
//    private final Runnable runnable;
//
//    private RunnableAdapter(DefaultTaskQueue parent,
//                            Runnable runnable) {
//
//      this.parent = parent;
//      this.runnable = runnable;
//    }
//
//    @Override
//    public void run() {
//
//      try {
//
//        runnable.run();
//      }
//      catch (Throwable t) {
//
//        UncaughtExceptionHandler h = parent.handler;
//        if( h != null )
//          h.uncaughtException(Thread.currentThread(), t);
//      }
//    }
//  }
//
//  private static class CallableAdapter<R> implements Runnable {
//
//    private final DefaultTaskQueue parent;
//    private final Callable<R> callable;
//    private final CountDownLatch cd;
//
//    private volatile R result;
//    private volatile Exception ex;
//
//    private CallableAdapter(DefaultTaskQueue parent,
//                            Callable<R> callable) {
//
//      this.parent = parent;
//      this.callable = callable;
//      this.cd = new CountDownLatch(1);
//    }
//
//    @Override
//    public void run() {
//
//      try {
//
//        R r = callable.call();
//        result = r;
//      }
//      catch (Throwable t) {
//
//        if( t instanceof Exception ) {
//
//          ex = (Exception)t;
//        }
//        else {
//
//          @SuppressWarnings("unchecked")
//          Error err = (Error)t;
//
//          UncaughtExceptionHandler h = parent.handler;
//          if( h != null )
//            h.uncaughtException(Thread.currentThread(), err);
//          else
//            throw err;
//        }
//      }
//      finally {
//
//        cd.countDown();
//      }
//    }
//
//    R get() throws Exception {
//
//      cd.await(); // potentially throws InterrptedException
//      if( ex != null )
//        throw ex;
//      return result;
//    }
//  }
}
