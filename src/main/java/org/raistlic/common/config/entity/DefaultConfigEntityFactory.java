package org.raistlic.common.config.entity;

import org.raistlic.common.codec.Deserializer;
import org.raistlic.common.codec.Deserializers;
import org.raistlic.common.codec.ValueConversionException;
import org.raistlic.common.config.exception.ConfigEntityCreationException;
import org.raistlic.common.config.exception.ConfigValueConvertException;
import org.raistlic.common.config.source.ConfigSource;
import org.raistlic.common.precondition.Expectations;
import org.raistlic.common.precondition.ExpectedCases;
import org.raistlic.common.precondition.Precondition;
import org.raistlic.common.predicate.Predicates;
import org.raistlic.common.reflection.ConstructorStream;
import org.raistlic.common.reflection.Methods;
import org.raistlic.common.reflection.Reflections;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Parameter;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Predicate;

/**
 * @author lei.c (2015-12-21)
 */
public class DefaultConfigEntityFactory implements ConfigEntityFactory {

  private final Map<Class<?>, Deserializer<?>> deserializers;

  public DefaultConfigEntityFactory() {

    deserializers = new ConcurrentHashMap<>();
  }

  @Override
  @SuppressWarnings("unchecked")
  public <E> E createConfigEntity(Class<E> configEntityType, ConfigSource configSource, String path) {

    Precondition.param(configSource, "configSource").notNull();
    Precondition.param(configEntityType, "configEntityType").notNull();

    ConfigEntity configEntity = Reflections.getAnnotation(configEntityType, ConfigEntity.class);
    Precondition.param(configEntity).notNull(ConfigEntity.class.getName() + " annotation missing on 'configEntityType'");

    try {

      E entity = null;

      Method factoryMethod = getConfigConstructorFactoryMethod(configEntityType);
      if (factoryMethod != null) {
        entity = (E) createConfigEntity(configSource, configEntity, factoryMethod);
      }

      if (entity == null) {
        Constructor<E> constructor = getConfigConstructor(configEntityType);
        if (constructor != null) {
          entity = createConfigEntity(configSource, configEntity, constructor);
        }
      }

      if (entity == null) {
        factoryMethod = getUsableFactoryMethod(configEntityType);
        if (factoryMethod != null) {
          entity = (E) createConfigEntity(configSource, configEntity, factoryMethod);
        }
      }

      if (entity == null) {
        Constructor<E> constructor = getUsableConstructor(configEntityType);
        if (constructor != null) {
          entity = createConfigEntity(configSource, configEntity, constructor);
        }
      }

      VALIDATOR.expect(entity).notNull(
              "Cannot find usable factory method or constructor: '" + configEntityType.getName() + "'");

      injectConfigProperties(configSource, configEntity, configEntityType, entity);
      return entity;
    }
    catch (Exception ex) {
      if (ex instanceof ConfigEntityCreationException) {
        throw (ConfigEntityCreationException) ex;
      }
      else {
        throw new ConfigEntityCreationException(ex);
      }
    }
  }

  @Override
  public <E> void registerDeserializer(Class<E> type, Deserializer<E> deserializer) {

    Precondition.param(deserializer, "deserializer").notNull();
    Precondition.param(type, "type").notNull();
    Precondition.param(type).matches(
            VALID_DESERIALIZE_CUSTOMIZABLE_TYPE_PREDICATE,
            "The de-serialize logic for type '" + type.getName() + "' cannot be customized."
    );

    deserializers.put(type, deserializer);
  }

  private Deserializer<?> getDeserializer(Class<?> type) {

    Deserializer<?> deserializer = FIXED_DESERIALIZERS.get(type);
    if (deserializer == null) {
      deserializer = deserializers.get(type);
    }
    return deserializer;
  }

  private Object createConfigEntity(ConfigSource configSource,
                                    ConfigEntity configEntity,
                                    Method factoryMethod) throws Exception {

    Parameter[] parameters = factoryMethod.getParameters();
    if (parameters.length == 0) {
      factoryMethod.setAccessible(true);
      return factoryMethod.invoke(null);
    }
    else {
      Object[] configValues = prepareParameterValues(configSource, configEntity, parameters);
      factoryMethod.setAccessible(true);
      return factoryMethod.invoke(null, configValues);
    }
  }

  private <E> E createConfigEntity(ConfigSource configSource,
                                   ConfigEntity configEntity,
                                   Constructor<E> constructor) throws Exception {

    if (constructor.getParameterCount() == 0) {
      constructor.setAccessible(true);
      return constructor.newInstance();
    }
    else {
      Parameter[] parameters = constructor.getParameters();
      Object[] configValues = prepareParameterValues(configSource, configEntity, parameters);
      constructor.setAccessible(true);
      return constructor.newInstance(configValues);
    }
  }

  private Object[] prepareParameterValues(ConfigSource configSource,
                                          ConfigEntity configEntity,
                                          Parameter[] parameters) {

    Object[] properties = new Object[parameters.length];
    for (int i = 0, len = parameters.length; i < len; i++) {

      Parameter parameter = parameters[i];
      ConfigProperty configProperty = parameter.getAnnotation(ConfigProperty.class);
      String configPropertyName = getConfigPropertyName(configEntity, configProperty, null);
      Class<?> parameterType = parameter.getType();
      properties[i] = getConfigValue(configSource, configPropertyName, parameterType);
    }
    return properties;
  }

  private <E> void injectConfigProperties(ConfigSource configSource,
                                          ConfigEntity configEntity,
                                          Class<E> configEntityType,
                                          E entity) throws Exception {

    Map<Field, ConfigProperty> fields = Reflections.getAnnotatedFields(
            configEntityType, ConfigProperty.class, false);
    for (Map.Entry<Field, ConfigProperty> entry : fields.entrySet()) {
      Field field = entry.getKey();
      ConfigProperty configProperty = entry.getValue();
      String configPropertyName = getConfigPropertyName(configEntity, configProperty, field.getName());
      Object value = getConfigValue(configSource, configPropertyName, field.getType());
      field.setAccessible(true);
      field.set(entity, value);
    }
  }

  private Object getConfigValue(ConfigSource configSource, String key, Class<?> type) {

    VALIDATOR.expect(key).notNull(
            "ConfigProperty annotation value for property type '" + type.getName() + "' is null."
    );
    VALIDATOR.expect(key).notEmpty(
            "ConfigProperty annotation value for property type '" + type.getName() + "' is empty."
    );

    String value = configSource.getString(key);
    VALIDATOR.assertThat(
            value != null || !type.isPrimitive(),
            "Config value not found for primitive " + type.getName() + "property '" + key
    );
    if (value == null) {
      return null;
    }

    Deserializer<?> deserializer = getDeserializer(type);
    VALIDATOR.expect(deserializer, "Deserializer for type '" + type.getName() + "'").notNull();

    try {
      return deserializer.decode(value);
    }
    catch (ValueConversionException ex) {
      throw new ConfigEntityCreationException(ex);
    }
  }

  private static String getConfigPropertyName(ConfigEntity configEntity,
                                              ConfigProperty configProperty,
                                              String fallbackName) {

    String configName = configProperty.value();
    if (configName.isEmpty()) {
      VALIDATOR.expect(fallbackName).notNull("Some of the config properties missing property name.");
      configName = fallbackName;
    }
    if (!configEntity.path().isEmpty()) {
      configName = configEntity.path() + "." + configName;
    }
    return configName;
  }

  private static Method getConfigConstructorFactoryMethod(Class<?> configEntityType) {

    return Arrays.asList(configEntityType.getDeclaredMethods())
            .stream()
            .filter(ANNOTATED_CONFIG_CONSTRUCTOR_FACTORY_METHOD)
            .filter(method -> configEntityType.isAssignableFrom(method.getReturnType()))
            .findFirst()
            .orElse(null);
  }

  @SuppressWarnings("unchecked")
  private static <E> Constructor<E> getConfigConstructor(Class<E> configEntityType) {

    return (Constructor<E>) Arrays.asList(configEntityType.getConstructors())
            .stream()
            .filter(ANNOTATED_CONFIG_CONSTRUCTOR)
            .findFirst()
            .orElse(null);
  }

  private static Method getUsableFactoryMethod(Class<?> configEntityType) {

    return Arrays.asList(configEntityType.getDeclaredMethods())
            .stream()
            .filter(method -> Modifier.isStatic(method.getModifiers()))
            .filter(method -> configEntityType.isAssignableFrom(method.getReturnType()))
            .filter(method -> method.getParameterCount() == 0)
            .findFirst()
            .orElse(null);
  }

  @SuppressWarnings("unchecked")
  private static <E> Constructor<E> getUsableConstructor(Class<E> configEntityType) {

    return (Constructor<E>) Arrays.asList(configEntityType.getConstructors())
            .stream()
            .filter(constructor -> constructor.getParameterCount() == 0)
            .findFirst()
            .orElse(null);
  }

  private static Map<Class<?>, Deserializer<?>> initFixedDeserializers() {

    Map<Class<?>, Deserializer<?>> map = new HashMap<>();
    map.put(String.class, Deserializers.getStringDeserializer());
    map.put(Boolean.class, Deserializers.getBooleanDeserializer());
    map.put(boolean.class, Deserializers.getBooleanDeserializer());
    map.put(Byte.class, Deserializers.getByteDeserializer());
    map.put(byte.class, Deserializers.getByteDeserializer());
    map.put(Character.class, Deserializers.getCharacterDeserializer());
    map.put(char.class, Deserializers.getCharacterDeserializer());
    map.put(Short.class, Deserializers.getShortDeserializer());
    map.put(short.class, Deserializers.getShortDeserializer());
    map.put(Integer.class, Deserializers.getIntegerDeserializer());
    map.put(int.class, Deserializers.getIntegerDeserializer());
    map.put(Long.class, Deserializers.getLongDeserializer());
    map.put(long.class, Deserializers.getLongDeserializer());
    map.put(Float.class, Deserializers.getFloatDeSerializer());
    map.put(float.class, Deserializers.getFloatDeSerializer());
    map.put(Double.class, Deserializers.getDoubleDeserializer());
    map.put(double.class, Deserializers.getDoubleDeserializer());
    map.put(BigInteger.class, Deserializers.getBigIntegerDeserializer());
    map.put(BigDecimal.class, Deserializers.getBigDecimalDeserializer());
    map.put(Integer.class, Deserializers.getStringDeserializer());
    return map;
  }

  private static final Map<Class<?>, Deserializer<?>> FIXED_DESERIALIZERS
          = Collections.unmodifiableMap(initFixedDeserializers());

  private static final ExpectedCases VALIDATOR = Expectations.with(ConfigValueConvertException::new);

  private static final Predicate<Class<?>> VALID_DESERIALIZE_CUSTOMIZABLE_TYPE_PREDICATE =
          aClass -> ! (aClass.isPrimitive() || FIXED_DESERIALIZERS.containsKey(aClass));

  private static final Predicate<Method> ANNOTATED_CONFIG_CONSTRUCTOR_FACTORY_METHOD =
          Predicates.<Method>builder(method -> Modifier.isStatic(method.getModifiers()))
                  .and(method -> method.isAnnotationPresent(ConfigConstructor.class))
                  .and(Methods.predicateParametersAnnotatedWith(ConfigProperty.class))
                  .get();

  private static final Predicate<Constructor<?>> ANNOTATED_CONFIG_CONSTRUCTOR =
          Predicates.<Constructor<?>>builder(constructor -> constructor.isAnnotationPresent(ConfigConstructor.class))
                  .and(ConstructorStream.predicateParametersAnnotatedWith(ConfigProperty.class))
                  .get();
}
