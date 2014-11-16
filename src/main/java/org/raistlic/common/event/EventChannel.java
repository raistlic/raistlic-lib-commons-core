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

import org.raistlic.common.ExceptionHandler;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * @author Lei CHEN (2014-11-13)
 * @since 1.0
 */
class EventChannel {

  private final String name;

  private final List<EventListenerAdapter> listenerAdapters;

  private final ExceptionHandler exceptionHandler;

  EventChannel(String name, ExceptionHandler exceptionHandler) {

    assert exceptionHandler != null : "'exceptionHandler' is null.";

    this.name = name;
    this.listenerAdapters = new CopyOnWriteArrayList<EventListenerAdapter>();
    this.exceptionHandler = exceptionHandler;
  }

  String name() {

    return name;
  }

  void add(EventListenerAdapter adapter) {

    if (name == null) {

      if (adapter.channelName() != null) {

        throw new IllegalArgumentException(
                "'adapter' s channel name " + adapter.channelName() +
                        " does not match the channel's name null"
        );
      }
    }
    else {

      if (!name.equals(adapter.channelName())) {

        throw new IllegalArgumentException(
                "'adpater' s channel name " + adapter.channelName() +
                        " does not match the channel's name " + name
        );
      }
    }
    listenerAdapters.add(adapter);
  }

  void remove(Object listener) {

    List<EventListenerAdapter> toBeRemoved = new ArrayList<EventListenerAdapter>();
    for (EventListenerAdapter adapter : listenerAdapters) {

      if (adapter.listener().equals(listener)) {

        toBeRemoved.add(adapter);
      }
    }
    listenerAdapters.removeAll(toBeRemoved);
  }

  void dispatch(Event event) {

    for (EventListenerAdapter adapter : listenerAdapters) {

      try {

        adapter.listen(event);
      }
      catch (Exception ex) {

        exceptionHandler.exceptionOccur(Thread.currentThread(), ex);
      }
    }
  }
}
