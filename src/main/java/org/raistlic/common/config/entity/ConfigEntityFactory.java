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

import org.raistlic.common.codec.Deserializer;
import org.raistlic.common.config.source.ConfigSource;

import java.util.Set;

/**
 * The interface defines contract of a config entity factory, which is capable of creating config
 * entity beans out of some {@link ConfigSource} .
 *
 * @author Lei Chen (2016-02-17)
 */
public interface ConfigEntityFactory {

  /**
   * The method creates a new config entity bean of the specified {@code configEntityType} , based
   * on the configuration values in the {@code configSource} .
   *
   * @param configEntityType the type of the config entity bean to create, cannot be {@code null}.
   * @param configSource the config source to be based on, cannot be {@code null}.
   * @param path the path prefix of the configuration properties for the config entity bean, cannot
   *             be {@code null}.
   * @param <E> the referenced type of the config entity bean to create.
   * @return the created config entity bean.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when any of the parameters
   *         is {@code null}.
   */
  <E> E createConfigEntity(Class<E> configEntityType, ConfigSource configSource, String path);

  /**
   * The method returns the config property names required to create an entity instance of the
   * specified {@code configEntityType}, as a {@link Set} of {@link String} .
   *
   * @param configEntityType the config entity type to analyse, cannot be {@code null}, and must be
   *                         a valid config entity type (i.e. cannot be a primitive type, must have
   *                         dependencies that are annotated with {@link ConfigProperty}).
   * @return the property name set required, or an empty set if none .
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code configEntityType}
   *         is {@code null} or is not a valid config entity type.
   */
  Set<String> getConfigKeys(Class<?> configEntityType);

  /**
   * Registers the {@code deserializer} for the specified {@code type}, which is to be used when
   * ever the factory de-serializes a value of the {@code type}.
   *
   * @param type the type to register the de-serializer for, cannot be {@code null}.
   * @param deserializer the de-serializer to register, cannot be {@code null}.
   * @param <E> the referenced type.
   */
  <E> void registerDeserializer(Class<E> type, Deserializer<E> deserializer);
}
