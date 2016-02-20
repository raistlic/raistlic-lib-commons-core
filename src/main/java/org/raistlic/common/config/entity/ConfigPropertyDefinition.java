package org.raistlic.common.config.entity;

import org.raistlic.common.precondition.Precondition;

import java.lang.reflect.Field;
import java.lang.reflect.Parameter;
import java.util.function.Function;

/**
 * @author Lei Chen (2016-02-02)
 */
public final class ConfigPropertyDefinition {

  public static ConfigPropertyDefinition of(Function<ConfigProperty, String> configPropertyNameStrategy, Parameter parameter) {

    Precondition.param(configPropertyNameStrategy, "configPropertyNameStrategy").notNull();
    Precondition.param(parameter, "parameter").notNull();

    ConfigProperty configProperty = parameter.getAnnotation(ConfigProperty.class);
    Precondition.param(configProperty, "parameter.configProperty annotation").notNull();

    String propertyName = configPropertyNameStrategy.apply(configProperty);
    Class<?> propertyType = parameter.getType();
    return new ConfigPropertyDefinition(propertyName, propertyType);
  }

  public static ConfigPropertyDefinition of(Function<ConfigProperty, String> configPropertyNameStrategy, Field field) {

    Precondition.param(configPropertyNameStrategy, "configPropertyNameStrategy").notNull();
    Precondition.param(field, "field").notNull();

    ConfigProperty configProperty = field.getAnnotation(ConfigProperty.class);
    Precondition.param(configProperty, "field.configProperty annotation").notNull();

    String propertyName = configPropertyNameStrategy.apply(configProperty);
    Class<?> propertyType = field.getType();
    return new ConfigPropertyDefinition(propertyName, propertyType);
  }

  private final String propertyName;

  private final Class<?> propertyType;

  private ConfigPropertyDefinition(String propertyName, Class<?> propertyType) {

    Precondition.param(propertyName, "propertyName").notNullOrEmpty();
    Precondition.param(propertyType, "propertyType").notNull();

    this.propertyName = propertyName;
    this.propertyType = propertyType;
  }

  public String getPropertyName() {

    return propertyName;
  }

  public Class<?> getPropertyType() {

    return propertyType;
  }

  @Override
  public boolean equals(Object obj) {

    if (obj == this) {
      return true;
    } else if (obj instanceof ConfigPropertyDefinition) {
      ConfigPropertyDefinition casted = (ConfigPropertyDefinition) obj;
      return this.propertyName.equals(casted.propertyName) && this.propertyType == casted.propertyType;
    } else {
      return false;
    }
  }

  @Override
  public int hashCode() {

    return propertyName.hashCode() * 31 + propertyType.hashCode();
  }
}
