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

package org.raistlic.common.config.source;

import java.util.Set;

/**
 * @author Lei Chen (2015-09-10)
 */
public interface ConfigSource {

  /**
   * The method returns all the keys that the {@link ConfigSource} contains, as a read-only
   * {@link java.util.Set} .
   *
   * @return all the keys that the {@link ConfigSource} contains as a read-only {@link java.util.Set} ,
   *         any attempts to modify the returned {@link java.util.Set} may cause an exception. Empty
   *         set returned in case the config source is empty, the method never returns {@code null}.
   */
  Set<String> getKeys();

  /**
   * The method queries whether the {@link ConfigSource} has the specified {@code key} .
   *
   * @param key the key to query, cannot be {@code null}.
   * @return {@code true} if the {@link ConfigSource} has the {@code key} .
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code key} is {@code null}.
   */
  boolean hasKey(String key);

  /**
   * The method queries the {@link String} value specified by {@code key} .
   *
   * @param key the key to search, cannot be {@code null}.
   * @return the {@link String} value specified by {@code key} in the {@link ConfigSource} ,
   *         or {@code null} if the {@code key} is not found.
   *
   * @throws org.raistlic.common.precondition.PreconditionException when {@code key} is {@code null}.
   */
  String getString(String key);
}

