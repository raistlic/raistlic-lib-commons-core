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

package org.raistlic.common;

/**
 * A callback interface to handle exceptions, especially in an asynchronous environment where
 * the caller may not be able to wait for the invocation target's execution.
 *
 * @author Lei CHEN (2013-11-29)
 * @since 1.0
 */
public interface ExceptionHandler {

  /**
   * Handle the specified {@code ex}, the method does not throw any exception, regardless of the
   * parameters passed in.
   *
   * @param thread the thread in which the exception occurred.
   * @param ex the exception occurred.
   */
  public void exceptionOccur(Thread thread, Throwable ex);
}
