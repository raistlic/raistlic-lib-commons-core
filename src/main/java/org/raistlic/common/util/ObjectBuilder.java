/*
 * Copyright 2016 Lei Chen (raistlic@gmail.com)
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

package org.raistlic.common.util;

import java.util.function.Supplier;

/**
 * Generic type for builder class in the builder pattern. Pre-exist Java 8 and adapted to {@link Supplier} since Java 8.
 *
 * @param <E> the instance type being built.
 */
public interface ObjectBuilder<E> extends Supplier<E> {

  /**
   * Construct and return instance with builder state.
   *
   * @return the built instance.
   */
  E build();

  default E get() {
    return build();
  }
}
