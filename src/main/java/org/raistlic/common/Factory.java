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
 * Generally a builder pattern instance can implement this interface, and be
 * passed around after it's state is set and is ready to export instances. This
 * interface is mainly used as a callback, to help decoupling instantiation
 * logic of the referenced type from other business logic of the client code.
 *
 * @author Lei CHEN (2013-11-29)
 * @since 1.0
 */
public interface Factory<T> {

  /**
   * This method returns an instance of the referenced type. Although the method
   * name says "build", whether a new instance is created on invocation depends
   * on the implementation.
   *
   * @return A newly created instance of the target(reference) type.
   *
   * @throws IllegalStateException if the factory is not ready, see {@link #isReady()}.
   */
  public T build();

  /**
   * A condition check method, to query whether the factory is ready to export
   * target type instances. The idea is that some factories (like a builder in a
   * Builder Design Pattern) may need some compulsory parameters set before its
   * {@link #build()} method can be safely called; failing to set these parameters
   * before calling {@link #build()} may cause an {@link IllegalStateException}.
   * <p/>
   * It is guaranteed that, if this method returns {@code true}, and before any
   * change happens to the factory state, its {@link #build()} method should
   * succeed(i.e. should not throw {@link IllegalStateException}).
   *
   * @return {@code true} if the factory is ready to export instances.
   */
  public boolean isReady();
}
