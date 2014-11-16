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

/**
 * The default implementation of {@link org.raistlic.common.event.EventDispatcher.Builder} .
 *
 * @author Lei CHEN (2014-11-15)
 * @since 1.0
 */
class DefaultEventDispatcherBuilder implements EventDispatcher.Builder {

  private String broadCastName;

  private ExceptionHandler exceptionHandler;

  DefaultEventDispatcherBuilder() {

    importConfig(DefaultEventDispatcherConfig.INSTANCE);
  }

  private void importConfig(EventDispatcherConfig config) {

    broadCastName = config.getBroadCastName();
    exceptionHandler = config.getExceptionHandler();
  }

  @Override
  public EventDispatcher.Builder withConfig(EventDispatcherConfig config) {

    if (config == null) {

      throw new NullPointerException("'config' is null.");
    }

    importConfig(config);
    return this;
  }

  @Override
  public EventDispatcher build() {

    return new DefaultEventDispatcher(exceptionHandler, broadCastName);
  }

  @Override
  public boolean isReady() {

    return true;
  }
}
