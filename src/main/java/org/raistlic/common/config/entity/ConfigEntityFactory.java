package org.raistlic.common.config.entity;

import org.raistlic.common.codec.Deserializer;
import org.raistlic.common.config.source.ConfigSource;

/**
 * @author Lei Chen (2016-02-17)
 */
public interface ConfigEntityFactory {

  <E> E createConfigEntity(Class<E> configEntityType, ConfigSource configSource, String path);

  <E> void registerDeserializer(Class<E> type, Deserializer<E> deserializer);
}
