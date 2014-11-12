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

package org.raistlic.common;

/**
 * A self executable service, that has a life cycle, and can be controlled by
 * calling its start, pause, resume and shutdown methods. This interface implies
 * that a service is likely run in a separate thread, and that an
 * {@link java.util.concurrent.ExecutorService} may already built in the Service
 * instance when it is created.
 *
 * <p/>
 * Note that when a {@link Service} instance is created, you may want to attach
 * an {@link ExceptionHandler} to it, --the interface itself does not provide
 * any set methods for registering an {@link ExceptionHandler} although almost any
 * service may need one, because it is not encouraged that the {@link ExceptionHandler}
 * can be switched after a {@link Service} is created.
 *
 * <p/>
 * See {@link ExceptionHandler}.
 *
 * @author Lei.C (13-11-9)
 */
public interface Service {

  /**
   * Start the service.
   *
   * @return {@code true} if the status of the service is changed to be 'Running'
   *         from another status, as a result of the call.
   *
   * @throws IllegalStateException if the service cannot start from the current
   *         status.
   */
  public boolean start();

  /**
   * Pause the service, depends on the specific implementation, the service may
   * be either shutdown(if it cannot be paused) or be turned into 'paused' state,
   * and ready to be 'resumed'.
   *
   * @return {@code true} if the status of the service is changed as a result of
   *         the call.
   */
  public boolean pause();

  /**
   * Resume the service, turn the service to be 'running' from the status 'paused'.
   *
   * @return {@code true} if the status of the service is changed as a result of
   *         the call.
   *
   * @throws IllegalStateException if the current status of the service is not
   *         'paused'.
   */
  public boolean resume();

  /**
   * Shutdown the service, free any resources the service might have occupied,
   * destroy it.
   *
   * @return {@code true} if the status successfully turned into 'shutting down'
   *         from another status, as a result of the call.
   */
  public boolean shutdown();

  /**
   * This method returns the current status of the Service.
   *
   * @return the current status of the task.
   */
  public Status status();

  public enum Status {

    Created,

    Running,

    Paused,

    ShuttingDown,

    Destroyed,

    ;
  }
}
