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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author Lei CHEN (2014-11-13)
 * @since 1.0
 */
class DefaultEventDispatcher implements EventDispatcher {

  private static final Logger LOGGER = LoggerFactory.getLogger(DefaultEventDispatcher.class);

  private final Map<String, EventChannel> eventChannelMap;

  private final EventChannel broadCastChannel;

  private final ExceptionHandler exceptionHandler;

  DefaultEventDispatcher(ExceptionHandler exceptionHandler,
                         String broadCastName) {

    assert exceptionHandler != null : "'exceptionHandler' is null.";
    assert broadCastName != null : "'broadCastName' is null.";

    this.exceptionHandler = exceptionHandler;
    this.eventChannelMap = new ConcurrentHashMap<String, EventChannel>();
    this.broadCastChannel = new EventChannel(broadCastName, exceptionHandler);
  }

  @Override
  public void post(Event event, String... channels) {

    if (event == null) {

      throw new NullPointerException("'event' is null.");
    }

    if (isBroadCastChannel(channels)) {

      doPostInAllChannels(event);

    }
    else {

      for (String channel : channels) {

        doPost(event, channel);
      }
    }
    broadCastChannel.dispatch(event);
  }

  private void doPost(Event event, String channelName) {

    EventChannel channel = eventChannelMap.get(channelName);
    if (channel != null) {

      channel.dispatch(event);
    }
  }

  private void doPostInAllChannels(Event event) {

    List<EventChannel> channels;
    synchronized (eventChannelMap) {

      channels = new ArrayList<EventChannel>(eventChannelMap.values());
    }
    for (EventChannel channel : channels) {

      channel.dispatch(event);
    }
  }

  @Override
  public void register(Object listener) {

    if (listener == null) {

      throw new NullPointerException("'listener' is null.");
    }

    List<EventListenerAdapter> adapters = createListenerAdapters(listener);
    for (EventListenerAdapter adapter : adapters) {

      EventChannel channel = getOrCreateChannel(adapter.channelName());
      channel.add(adapter);
    }
  }

  @Override
  public void register(Collection<?> listeners) {

    if (listeners == null) {

      throw new NullPointerException("'listeners' is null.");
    }
    for (Object listener : listeners) {

      register(listener);
    }
  }

  private List<EventListenerAdapter> createListenerAdapters(Object listener) {

    Class<?> listenerType = listener.getClass();
    List<EventListenerAdapter> list = new ArrayList<EventListenerAdapter>();
    createListenerAdaptersRecursive(list, listener, listenerType);
    return list;
  }

  private void createListenerAdaptersRecursive(List<EventListenerAdapter> list,
                                               Object listener,
                                               Class<?> listenerType) {

    for (Method method : listenerType.getDeclaredMethods()) {

      if (!isMethodValid(method)) {

        continue;
      }
      registerMethodAsAdapters(list, listener, method);
    }

    Class<?> parentType = listenerType.getSuperclass();
    if (parentType != null) {

      createListenerAdaptersRecursive(list, listener, parentType);
    }
  }

  private void registerMethodAsAdapters(List<EventListenerAdapter> list,
                                        Object listener,
                                        Method method) {

    Subscribe annotation = method.getAnnotation(Subscribe.class);
    Class<?> eventType = method.getParameterTypes()[0];
    String[] channels = annotation.value();

    if (isBroadCastChannel(channels)) {

      EventListenerAdapter adapter = new EventListenerAdapter(listener, method, eventType, null);
      list.add(adapter);
      return;
    }

    for (String channel : channels) {

      if (channel == null) {

        LOGGER.warn("null channel name found in the method annotation: '" +
                             method.getDeclaringClass().getName() + "." +
                             method.getName() + ", and is ignored."
        );
        continue;
      }
      EventListenerAdapter adapter = new EventListenerAdapter(listener, method, eventType, channel);
      list.add(adapter);
    }
  }

  private boolean isMethodValid(Method method) {

    Subscribe annotation = method.getAnnotation(Subscribe.class);
    if (annotation == null) {

      return false;
    }

    int modifier = method.getModifiers();
    if (Modifier.isStatic(modifier)) {

      LOGGER.warn("Subscribe annotation found on static method '" +
                          method.getDeclaringClass().getName() + "." +
                          method.getName() + "' and is ignored, static method " +
                          "subscription is not supported."
      );
      return false;
    }

    Class<?>[] paramTypes = method.getParameterTypes();
    if (paramTypes == null || paramTypes.length != 1) {

      LOGGER.warn("Method '" + method.getDeclaringClass().getName() + "." + method.getName() +
                          "' is annotated as subscription method, but does not match parameter " +
                          "criteria, thus is not registered. subscription method should have " +
                          "exactly one parameter, which is of type '" + Event.class.getName() +
                          "' or its sub-type."
      );
      return false;
    }
    Class<?> eventType = paramTypes[0];

    if (!Event.class.isAssignableFrom(eventType)) {

      LOGGER.warn("Method '" + method.getDeclaringClass().getName() + "." + method.getName() +
                          "' is annotated as subscription method, but does not match parameter " +
                          "criteria, thus is not registered: its parameter type '" +
                          eventType.getName() + "' is not a subtype of '" + Event.class.getName() + "'."
      );
      return false;
    }

    try {

      method.setAccessible(true);

    }
    catch (Exception ex) {

      LOGGER.warn("Method not accessible: '" + method.getDeclaringClass().getName() + "." +
                          method.getName() + " and thus is not registered.", ex
      );
      return false;
    }
    return true;
  }

  private EventChannel getOrCreateChannel(String channelName) {

    if (isBroadCastChannel(channelName)) {

      return broadCastChannel;
    }

    EventChannel channel = eventChannelMap.get(channelName);
    if (channel == null) synchronized (eventChannelMap) {

      channel = eventChannelMap.get(channelName);
      if (channel == null) {

        channel = new EventChannel(channelName, exceptionHandler);
        eventChannelMap.put(channelName, channel);
      }
    }
    return channel;
  }

  @Override
  public void unregister(Object listener) {

    throw new UnsupportedOperationException();
  }

  @Override
  public void unregister(Collection<?> listeners) {

    if (listeners == null) {

      throw new NullPointerException("'listeners' is null.");
    }
    for (Object listener : listeners) {

      unregister(listener);
    }
  }

  @Override
  public void unregisterAll() {

    synchronized (eventChannelMap) {

      eventChannelMap.clear();
    }
  }

  private boolean isBroadCastChannel(String[] channelNames) {

    if (channelNames == null || channelNames.length == 0) {

      return true;
    }

    int validChannelNameCount = 0;
    for (String channelName : channelNames) {

      if (isBroadCastChannel(channelName)) {

        return true;
      }
      if (channelName != null) {

        validChannelNameCount ++;
      }
    }
    return validChannelNameCount > 0;
  }

  private boolean isBroadCastChannel(String channelName) {

    String broadCastName = broadCastChannel.name();

    if (broadCastName == null) {

      return channelName == null;

    }
    else {

      return broadCastName.equals(channelName);
    }
  }
}
