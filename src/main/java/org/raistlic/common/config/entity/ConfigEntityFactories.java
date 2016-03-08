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

package org.raistlic.common.config.entity;

/**
 * The class is a static factory method holder for {@link ConfigEntityFactory} interface.
 *
 * @author Lei Chen (2016-03-08)
 */
public final class ConfigEntityFactories {

  /**
   * The method creates and returns an instance of the default implementation of the
   * {@link ConfigEntityFactory} interface.
   *
   * @return the newly created instance.
   */
  public static ConfigEntityFactory newConfigEntityFactory() {

    return new ConfigEntityFactoryDefault();
  }

  /*
   * Not to be instantiated or inherited.
   */
  private ConfigEntityFactories() { }
}
