/*
 * Copyright 2014 Lei CHEN (raistlic@gmail.com)
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

package org.raistlic.common.event;

import org.raistlic.common.taskqueue.TaskQueue;

import java.util.concurrent.ExecutorService;

/**
 * The class defines a collection of static methods which help instantiating {@link EventDispatcher}s.
 *
 * @author Lei CHEN (2014-11-12)
 * @since 1.0
 */
public final class EventDispatchers {

  /**
   * The name of the 'broadcast' channel, which receives all channels' events.
   */
  public static final String DEFAULT_BROADCAST_NAME = "*";

  /**
   * The default event dispatcher configuration, used on absence of the parameter.
   */
  public static final EventDispatcherConfig DEFAULT_CONFIG = DefaultEventDispatcherConfig.INSTANCE;

  public static EventDispatcher.Builder newBuilder() {

    return new DefaultEventDispatcherBuilder();
  }

  public static EventDispatcher newInstance() {

    return newInstance(DEFAULT_CONFIG);
  }

  public static EventDispatcher newInstance(EventDispatcherConfig config) {

    if (config == null) {

      throw new NullPointerException("'config' is null.");
    }
    return newBuilder().withConfig(config).build();
  }

  public static EventDispatcher asyncProxyOf(EventDispatcher eventDispatcher, TaskQueue taskQueue) {

    return null;
  }

  public static EventDispatcher asyncProxyOf(EventDispatcher eventDispatcher, ExecutorService executorService) {

    return null;
  }

  /**
   * The class is a static method holder, and is not to be instantiated or inherited.
   */
  private EventDispatchers() {

  }
}
