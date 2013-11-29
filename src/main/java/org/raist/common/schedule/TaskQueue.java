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

import org.raist.common.Task;

import java.util.concurrent.Callable;

/**
 *
 * @author Lei CHEN
 */
public interface TaskQueue extends Task {
  
  public void schedule(Runnable runnable);
  
  public <E> E scheduleAndWait(Callable<E> callable) throws Exception;
  
  public void setExceptionHandler(Thread.UncaughtExceptionHandler handler);
}
