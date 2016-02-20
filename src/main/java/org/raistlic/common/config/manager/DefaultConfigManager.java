package org.raistlic.common.config.manager;

import org.raistlic.common.codec.Deserializer;
import org.raistlic.common.config.core.AbstractMutableConfigDecorator;
import org.raistlic.common.config.core.Configurable;
import org.raistlic.common.config.core.MutableConfig;
import org.raistlic.common.config.entity.DefaultConfigEntityFactory;
import org.raistlic.common.precondition.Precondition;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArraySet;

/**
 * @author Lei Chen (2016-02-02)
 */
class DefaultConfigManager extends AbstractMutableConfigDecorator implements ConfigManager {

  private final DefaultConfigEntityFactory configEntityFactory;

  private final Map<Class<?>, Set<Configurable<?>>> configurableMap;

  private final Map<String, Class<?>> configKeyTypeMap;

  public DefaultConfigManager(MutableConfig mutableConfig,
                              DefaultConfigEntityFactory configEntityFactory) {

    super(mutableConfig);

    Precondition.param(configEntityFactory, "configEntityFactory").notNull();

    this.configEntityFactory = configEntityFactory;
    this.configurableMap = new ConcurrentHashMap<>();
    this.configKeyTypeMap = new ConcurrentHashMap<>();
  }

  @Override
  public <B> B getConfigEntity(Class<B> entityType) {

    Precondition.param(entityType, "entityType").notNull();
    return configEntityFactory.createConfigEntity(mutableConfig, entityType);
  }

  @Override
  public <E> void registerDeserializer(Class<E> type, Deserializer<E> deserializer) {

    configEntityFactory.registerDeserializer(type, deserializer);
  }

  @Override
  public <C> void registerConfigurable(Class<C> configType, Configurable<C> configurable) {

    Precondition.param(configType, "configType").notNull();
    Precondition.param(configurable, "configurable").notNull();

    synchronized (configurableMap) {
      Set<Configurable<?>> configurableSet = configurableMap.get(configType);
      if (configurableSet == null) {
        configurableSet = new CopyOnWriteArraySet<>();
        configurableMap.put(configType, configurableSet);
      }
      configurableSet.add(configurable);
    }
  }

  @Override
  public <C> void removeConfigurable(Class<C> configType, Configurable<C> configurable) {

    Precondition.param(configType, "configType").notNull();
    Precondition.param(configurable, "configurable").notNull();

    synchronized (configurableMap) {
      Set<Configurable<?>> configurableSet = configurableMap.get(configType);
      if (configurableSet != null) {
        configurableSet.remove(configurable);
      }
    }
  }
}
