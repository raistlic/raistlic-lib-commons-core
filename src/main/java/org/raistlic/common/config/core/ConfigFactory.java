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

package org.raistlic.common.config.core;

import org.raistlic.common.config.source.ConfigSource;
import org.raistlic.common.precondition.Precondition;

/**
 * @author Lei Chen (2015-09-11)
 */
public final class ConfigFactory {

  public static Config wrap(ConfigSource configSource) {

    Precondition.param(configSource).isNotNull();
    return new ConfigSourceWrapper(configSource);
  }

  public static MutableConfig newMutableConfig() {

    return new MutableConfigDefault();
  }

  private ConfigFactory() { }
}
