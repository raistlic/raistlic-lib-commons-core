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
import org.raistlic.common.config.core.AbstractMutableConfigDecorator;
import org.raistlic.common.config.core.Configurable;
import org.raistlic.common.config.core.MutableConfig;
import org.raistlic.common.config.entity.ConfigEntityFactory;
import org.raistlic.common.precondition.Precondition;

import java.util.*;

/**
 * The default implementation of {@link ConfigManager} interface.
 *
 * @author Lei Chen (2016-02-02)
 */
class ConfigManagerDefault extends AbstractMutableConfigDecorator implements ConfigManager {

  private final Object lock;

  private final ConfigEntityFactory configEntityFactory;

  private final Map<String, Set<Class<?>>> configEntityMap;

  private final Map<Class<?>, Set<Configurable<?>>> configurableMap;

  ConfigManagerDefault(MutableConfig mutableConfig,
                       ConfigEntityFactory configEntityFactory) {

    super(mutableConfig);

    Precondition.param(configEntityFactory).isNotNull();

    this.lock = new Object();
    this.configEntityFactory = configEntityFactory;
    this.configEntityMap = new HashMap<>();
    this.configurableMap = new HashMap<>();
  }

  @Override
  public <B> B getConfigEntity(Class<B> entityType, String path) {

    Precondition.param(entityType).isNotNull();
    if (entityType.isInstance(this)) {
      return entityType.cast(this);
    }
    return configEntityFactory.createConfigEntity(entityType, mutableConfig, path);
  }

  @Override
  public <E> void registerDeserializer(Class<E> type, Deserializer<E> deserializer) {

    configEntityFactory.registerDeserializer(type, deserializer);
  }

  @Override
  public <C> void registerConfigurable(Configurable<C> configurable) {

    Precondition.param(configurable).isNotNull();

    Class<C> configType = configurable.getConfigType();
    synchronized (lock) {
      Set<Configurable<?>> configurables = getConfigurables(configType);
      configurables.add(configurable);
    }
  }

  private Set<Configurable<?>> getConfigurables(Class<?> configEntityType) {

    Set<Configurable<?>> set = configurableMap.get(configEntityType);
    if (set == null) {
      registerConfigEntityType(configEntityType);
      set = new HashSet<>();
      configurableMap.put(configEntityType, set);
    }
    return set;
  }

  private void registerConfigEntityType(Class<?> configEntityType) {

    Set<String> keys = getRelatedKeys(configEntityType);
    for (String key : keys) {
      Set<Class<?>> classSet = configEntityMap.get(key);
      if (classSet == null) {
        classSet = new HashSet<>();
        configEntityMap.put(key, classSet);
      }
      classSet.add(configEntityType);
    }
  }

  private Set<String> getRelatedKeys(Class<?> configEntityType) {

    if (configEntityType.isInstance(this)) {
      return Collections.unmodifiableSet(this.getKeys());
    } else {
      return configEntityFactory.getConfigKeys(configEntityType);
    }
  }

  @Override
  public <C> void removeConfigurable(Configurable<C> configurable) {

    Precondition.param(configurable).isNotNull();

    Class<C> configType = configurable.getConfigType();
    synchronized (lock) {
      Set<Configurable<?>> configurableSet = configurableMap.get(configType);
      if (configurableSet != null) {
        configurableSet.remove(configurable);
      }
    }
  }

  @Override
  public void updateAllConfigurables() {

    synchronized (lock) {
      updateConfig(configurableMap.keySet());
    }
  }

  @Override
  protected void configValueUpdated(String key) {

    Precondition.assertParam(key != null, "'key' cannot be null.");

    synchronized (lock) {
      Set<Class<?>> configEntityTypes = Optional.ofNullable(configEntityMap.get(key))
        .orElse(Collections.emptySet());
      updateConfig(configEntityTypes);
    }
  }

  @SuppressWarnings({"unchecked", "rawtypes"})
  private void updateConfig(Set<Class<?>> configEntityTypes) {

    configEntityTypes.forEach(configEntityType -> {
      Object configEntity = getConfigEntity(configEntityType, "");
      for (Configurable configurable : configurableMap.get(configEntityType)) {
        configurable.applyConfig(configEntity);
      }
    });
  }

  @Override
  protected void configValuesUpdated(Iterable<String> keys) {

    Precondition.assertParam(keys != null, "'keys' cannot be null.");

    synchronized (lock) {
      Set<Class<?>> configEntityTypes = new HashSet<>();
      keys.forEach(key -> {
        Set<Class<?>> set = Optional.ofNullable(configEntityMap.get(key)).orElse(Collections.emptySet());
        configEntityTypes.addAll(set);
      });
      updateConfig(configEntityTypes);
    }
  }
}
