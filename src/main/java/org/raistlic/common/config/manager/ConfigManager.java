package org.raistlic.common.config.manager;

import org.raistlic.common.codec.Deserializer;
import org.raistlic.common.config.core.Configurable;
import org.raistlic.common.config.core.MutableConfig;

/**
 * @author Lei Chen (2016-02-02)
 */
public interface ConfigManager extends MutableConfig {

  <E> E getConfigEntity(Class<E> entityType);

  <E> void registerDeserializer(Class<E> type, Deserializer<E> deserializer);

  <C> void registerConfigurable(Class<C> configType, Configurable<C> configurable);

  <C> void removeConfigurable(Class<C> configType, Configurable<C> configurable);
}
