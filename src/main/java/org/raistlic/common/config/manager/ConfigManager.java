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

package org.raistlic.common.config.manager;

import org.raistlic.common.codec.Deserializer;
import org.raistlic.common.config.core.Configurable;
import org.raistlic.common.config.core.MutableConfig;

/**
 * The config manager is a {@link MutableConfig} instance that notifies registered (interested)
 * parties when it's configuration value is updated.
 *
 * @author Lei Chen (2016-02-02)
 */
public interface ConfigManager extends MutableConfig {

  /**
   * The method returns a config entity instance that holds values from the current configuration
   * context of the {@link ConfigManager} .
   *
   * @param entityType the class of the config entity, cannot be {@code null}.
   * @param <E>        the actual type of the config entity.
   * @return the config entity instance.
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code entityType} is
   *                                                                    {@code null}.
   */
  default <E> E getConfigEntity(Class<E> entityType) {

    return getConfigEntity(entityType, "");
  }

  /**
   * The method returns a config entity instance that holds values from the current configuration
   * context of the {@link ConfigManager} , using the specified {@code path} as a prefix of all
   * the configuration values to use.
   * <p>
   * See also {@link org.raistlic.common.config.entity.ConfigEntityFactory#registerDeserializer(Class, Deserializer)}.
   *
   * @param entityType the class of the config entity, cannot be {@code null}.
   * @param path       the prefix of all the config property names to be used, can be {@code null} or
   *                   empty in which case no prefix is used for the config property names.
   * @param <E>        the actual type of the config entity.
   * @return the config entity instance.
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code entityType} is
   *                                                                    {@code null}.
   */
  <E> E getConfigEntity(Class<E> entityType, String path);

  /**
   * The method registers a {@code deserializer} for the specified {@code type} , which is to be used
   * when fetching configuration values.
   *
   * @param type         the type to register the de-serializer for, cannot be {@code null}, cannot be
   *                     any primitive box type or {@link String} , as these types will only use the built-in
   *                     deserializer of the implementation.
   * @param deserializer the deserializer to be registered.
   * @param <E>          the actual type for which the deserializer is registered for.
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code type} or
   *                                                                    {@code deserializer} is {@code null}, or when the {@code type} cannot use custom
   *                                                                    deserializers.
   */
  <E> void registerDeserializer(Class<E> type, Deserializer<E> deserializer);

  /**
   * The method registers the {@code configurable} so that when related configuration values
   * are updated, its {@link Configurable#applyConfig(Object)} method will be called with the
   * latest configuration values.
   *
   * @param configurable the configurable to be registered, cannot be {@code null}.
   * @param <C>          the actual type of config entity that the {@code configurable} can be applied with.
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code configurable}
   *                                                                    is {@code null}.
   */
  <C> void registerConfigurable(Configurable<C> configurable);

  /**
   * The method removes the specified {@code configurable} from registration so that it will no
   * longer receive updates from the {@link ConfigManager} .
   *
   * @param configurable the configurable to be removed, cannot be {@code null}.
   * @param <C>          the actual type of config entity that the {@code configurable} can be applied with.
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code cofigurable}
   *                                                                    is {@code null}.
   */
  <C> void removeConfigurable(Configurable<C> configurable);

  /**
   * The method triggers an update all for all the registered {@link Configurable} s, regardless
   * of whether any configuration value is updated or not.
   */
  void updateAllConfigurables();
}
