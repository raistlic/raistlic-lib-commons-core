package org.raistlic.common.config.entity;

import org.raistlic.common.precondition.InvalidParameterException;
import org.raistlic.common.precondition.Precondition;
import org.raistlic.common.reflection.Reflections;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

/**
 * @author Lei Chen (2016-02-02)
 */
public final class ConfigEntityDefinition<E> {

  public static <E> ConfigEntityDefinition<E> of(ConfigEntity configEntity, Class<E> entityClass) {

    Precondition.param(configEntity, "configEntity").notNull();
    Precondition.param(entityClass, "entityClass").notNull();

    Method factoryMethod = getUsableFactoryMethod(entityClass);
    Constructor<E> constructor = getUsableConstructor(entityClass);

    Parameter[] parameters;
    if (factoryMethod != null) {
      parameters = factoryMethod.getParameters();
    } else if (constructor != null) {
      parameters = constructor.getParameters();
    } else {
      throw new InvalidParameterException("No usable factory method or constructor found for config entity type '" + entityClass.getName() + "'");
    }

    Function<ConfigProperty, String> configPropertyNameStrategy = new PropertyNameStrategy(configEntity);
    List<ConfigPropertyDefinition> parameterDefinitionList =
        configPropertyDefinitionListOf(configPropertyNameStrategy, parameters);
    List<ConfigFieldDefinition> fieldDefinitionList =
        configFieldDefinitionListOf(configPropertyNameStrategy, entityClass);

    return new ConfigEntityDefinition<>(entityClass, factoryMethod, constructor, parameterDefinitionList, fieldDefinitionList);
  }

  private static Method getUsableFactoryMethod(Class<?> entityClass) {

    return Reflections.methodStreamOf(entityClass)
            .staticOnes()
            .hasReturnType(entityClass)
            .annotatedWith(ConfigConstructor.class)
            .hasAllParametersMatch(CONFIG_PROPERTY_PARAMETER_PREDICATE)
            .findFirst()
            .orElse(null);
  }

  private static <E> Constructor<E> getUsableConstructor(Class<E> entityClass) {

    return Reflections.constructorStreamOf(entityClass)
        .annotatedWith(ConfigConstructor.class)
        .hasAllParametersMatch(CONFIG_PROPERTY_PARAMETER_PREDICATE)
        .findFirst()
        .orElse(null);
  }

  private static List<ConfigPropertyDefinition> configPropertyDefinitionListOf(
          Function<ConfigProperty, String> configPropertyNameStrategy, Parameter[] parameters) {

    if (parameters.length == 0) {
      return Collections.emptyList();
    } else {
      return Arrays.asList(parameters)
              .stream()
              .map(parameter -> ConfigPropertyDefinition.of(configPropertyNameStrategy, parameter))
              .collect(Collectors.toList());
    }
  }

  private static List<ConfigFieldDefinition> configFieldDefinitionListOf(
      Function<ConfigProperty, String> configPropertyNameStrategy, Class<?> entityType) {

    return Reflections.fieldStreamOf(entityType)
        .nonStaticOnes()
        .annotatedWith(ConfigProperty.class)
        .map(field -> new ConfigFieldDefinition(ConfigPropertyDefinition.of(configPropertyNameStrategy, field), field))
        .collect(Collectors.toList());
  }

  private final Class<E> entityType;

  private final Method usableFactoryMethod;

  private final Constructor<E> usableConstructor;

  private final List<ConfigPropertyDefinition> parameterDefinitionList;

  private final List<ConfigFieldDefinition> fieldDefinitionList;

  private ConfigEntityDefinition(Class<E> entityType,
                                 Method usableFactoryMethod,
                                 Constructor<E> usableConstructor,
                                 List<ConfigPropertyDefinition> parameterDefinitionList,
                                 List<ConfigFieldDefinition> fieldDefinitionList) {

    this.entityType = entityType;
    this.usableFactoryMethod = usableFactoryMethod;
    this.usableConstructor = usableConstructor;
    this.parameterDefinitionList = Collections.unmodifiableList(parameterDefinitionList);
    this.fieldDefinitionList = Collections.unmodifiableList(fieldDefinitionList);
  }

  public Class<E> getEntityType() {

    return entityType;
  }

  public Method getUsableFactoryMethod() {

    return usableFactoryMethod;
  }

  public Constructor<E> getUsableConstructor() {

    return usableConstructor;
  }

  public List<ConfigPropertyDefinition> getParameterDefinitionList() {

    return parameterDefinitionList;
  }

  public List<ConfigFieldDefinition> getFieldDefinitionList() {

    return fieldDefinitionList;
  }

  private static final class PropertyNameStrategy implements Function<ConfigProperty, String> {

    private final String prefix;

    private PropertyNameStrategy(ConfigEntity configEntity) {

      String path = configEntity.path();
      if (path.isEmpty()) {
        prefix = "";
      } else {
        prefix = path + ".";
      }
    }

    @Override
    public String apply(ConfigProperty configProperty) {

      return prefix + configProperty.value();
    }
  }

  private static final Predicate<Parameter> CONFIG_PROPERTY_PARAMETER_PREDICATE =
          parameter -> parameter.getAnnotation(ConfigProperty.class) != null;
}
