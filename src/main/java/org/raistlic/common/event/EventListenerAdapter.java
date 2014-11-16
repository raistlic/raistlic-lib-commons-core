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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * A reflection based listener adapter, which is used by the event channels.
 *
 * @author Lei CHEN (2014-11-13)
 * @since 1.0
 */
class EventListenerAdapter {

  private final Method method;

  private final Object listener;

  private final Class<?> eventType;

  private final String channelName;

  EventListenerAdapter(Object listener, Method method, Class<?> eventType, String channelName) {

    this.listener = listener;
    this.method = method;
    this.eventType = eventType;
    this.channelName = channelName;
  }

  Object listener() {

    return listener;
  }

  String channelName() {

    return channelName;
  }

  void listen(Event event) throws InvocationTargetException, IllegalAccessException, SecurityException {

    assert event != null : "'event' is null.";

    if (eventType.isInstance(event)) {

      method.invoke(listener, event);
    }
  }
}
