package org.raistlic.common.config.entity;

import org.raistlic.common.precondition.Precondition;

import java.lang.reflect.Field;
import java.util.Objects;

/**
 * @author Lei Chen (2016-02-03)
 */
public final class ConfigFieldDefinition {

  private final ConfigPropertyDefinition configPropertyDefinition;

  private final Field field;

  public ConfigFieldDefinition(ConfigPropertyDefinition configPropertyDefinition, Field field) {

    Precondition.param(configPropertyDefinition, "configPropertyDefinition").notNull();
    Precondition.param(field, "field").notNull();

    this.configPropertyDefinition = configPropertyDefinition;
    this.field = field;
  }

  public ConfigPropertyDefinition getConfigPropertyDefinition() {

    return configPropertyDefinition;
  }

  public Field getField() {

    return field;
  }

  @Override
  public boolean equals(Object o) {

    if (this == o) {
      return true;
    }
    if (!(o instanceof ConfigFieldDefinition)) {
      return false;
    }
    ConfigFieldDefinition that = (ConfigFieldDefinition) o;
    return Objects.equals(configPropertyDefinition, that.configPropertyDefinition) &&
            Objects.equals(field, that.field);
  }

  @Override
  public int hashCode() {

    return Objects.hash(configPropertyDefinition, field);
  }
}
