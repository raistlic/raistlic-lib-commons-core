package org.raistlic.common.config.entity;

import org.raistlic.common.codec.Deserializer;
import org.raistlic.common.codec.Deserializers;
import org.raistlic.common.config.core.Config;
import org.raistlic.common.config.core.ConfigBuilder;
import org.raistlic.common.config.core.ConfigFactory;
import org.raistlic.common.config.core.Configurable;
import org.raistlic.common.config.exception.ConfigEntityCreationException;
import org.raistlic.common.config.source.ConfigSourceFactory;
import org.raistlic.common.precondition.Precondition;
import org.raistlic.common.predicate.Predicates;
import org.raistlic.common.reflection.Constructors;
import org.raistlic.common.reflection.Methods;
import org.raistlic.common.reflection.Reflections;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Predicate;

/**
 * @author lei.c (2015-12-21)
 */
public class ConfigEntityFactory implements Configurable {

  private final Map<Class<?>, Deserializer<?>> deserializers;

  private final Map<Class<?>, Deserializer<?>> fixedDeserializers;

  private final Predicate<Class<?>> validDeserializeCustomizableTypePredicate;

  private volatile Config config;

  public ConfigEntityFactory() {

    config = ConfigFactory.wrap(ConfigSourceFactory.immutableEmptySource());
    deserializers = new ConcurrentHashMap<>();
    fixedDeserializers = Collections.unmodifiableMap(initFixedDeserializers());
    validDeserializeCustomizableTypePredicate = this.new ValidDeserializeCustomizableTypePredicate();
  }

  private Map<Class<?>, Deserializer<?>> initFixedDeserializers() {

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

  public <E> void registerDeserializer(Class<E> type, Deserializer<E> deserializer) {

    Precondition.param(deserializer, "deserializer").notNull();
    Precondition.param(type, "type").notNull();
    Precondition.param(type).matches(
            validDeserializeCustomizableTypePredicate,
            "The de-serialize logic for type '" + type.getName() + "' cannot be customized."
    );

    deserializers.put(type, deserializer);
  }

  private Deserializer<?> getDeserializer(Class<?> type) {

    Deserializer<?> deserializer = fixedDeserializers.get(type);
    if (deserializer == null) {
      deserializer = deserializers.get(type);
    }
    return deserializer;
  }

  @Override
  public void applyConfig(Config configuration) {

    Precondition.param(configuration, "configuration").notNull();

    ConfigBuilder builder = ConfigFactory.newMutableConfig();
    builder.applyConfig(config);
    builder.applyConfig(configuration);
    config = builder.build();
  }

  @Override
  public void extractConfig(ConfigBuilder builder) {

    Precondition.param(builder, "builder").notNull();

    builder.applyConfig(config);
  }

  public <E> E createConfigEntity(Class<E> configEntityType) {

    Precondition.param(configEntityType, "configEntityType").notNull();
    Precondition.param(configEntityType, "configEntityType").matches(ValidConfigEntityType.INSTANCE);

    Config configSnapshot = config;
    try {
      ConfigEntity entityAnnotation = Reflections.getAnnotation(configEntityType, ConfigEntity.class);
      E entity = configEntityType.newInstance();
      Map<Field, ConfigProperty> fields = Reflections.getAnnotatedFields(
              configEntityType, ConfigProperty.class, false);
      for (Field field : fields.keySet()) {
        ConfigProperty propertyAnnotation = fields.get(field);
        setConfigPropertyField(entity, configSnapshot, field, entityAnnotation, propertyAnnotation);
      }
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

  private void setConfigPropertyField(
          Object entity,
          Config c,
          Field field,
          ConfigEntity entityAnnotation,
          ConfigProperty propertyAnnotation) throws IllegalAccessException {

    String configName = propertyAnnotation.value();
    if (configName.isEmpty()) {
      configName = field.getName();
    }
    if (!entityAnnotation.path().isEmpty()) {
      configName = entityAnnotation.path() + "." + configName;
    }

    Class<?> fieldType = field.getType();
    Deserializer<?> deserializer = getDeserializer(fieldType);
    if (deserializer == null) {
      throw new ConfigEntityCreationException(
              "De-serializer not found for type '" + fieldType.getName() + "' of field '" +
                      field.getName() + "' with config name '" + configName + "'");
    }
    String value = c.getString(configName, null);
    if (value != null) {
      Object deserialized = deserializer.decode(value);
      field.setAccessible(true);
      field.set(entity, deserialized);
    }
  }

  private enum ValidConfigEntityType implements Predicate<Class<?>> {

    INSTANCE;

    private final Predicate<Method> USABLE_STATIC_FACTORY_METHOD_PARAMETERS_PREDICATE =
            Predicates.or(
                    Methods.predicateWithNoParameter(),
                    Predicates.and(
                            method -> method.isAnnotationPresent(ConfigConstructor.class),
                            Methods.predicateParametersAnnotatedWith(ConfigProperty.class)
                    )
            );

    private final Predicate<Constructor<?>> USABLE_CONSTRUCTOR_PREDICATE =
            Predicates.or(
                    Constructors.predicateWithNoParameter(),
                    Predicates.and(
                            constructor -> constructor.isAnnotationPresent(ConfigConstructor.class),
                            Constructors.predicateParametersAnnotatedWith(ConfigProperty.class)
                    )
            );

    @Override
    public boolean test(Class<?> aClass) {

      if (aClass == null) {
        return false;
      }
      if (!hasConfigEntityAnnotationOnType(aClass)) {
        return false;
      }
      return hasUsableConstructor(aClass) || hasUsableStaticFactoryMethod(aClass);
    }

    private boolean hasConfigEntityAnnotationOnType(Type type) {

      ConfigEntity configEntity = Reflections.getAnnotation(type, ConfigEntity.class);
      return configEntity != null;
    }

    private boolean hasUsableConstructor(Class<?> type) {

      for (Constructor<?> constructor : type.getConstructors()) {
        if (USABLE_CONSTRUCTOR_PREDICATE.test(constructor)) {
          return true;
        }
      }
      return false;
    }

    private boolean hasUsableStaticFactoryMethod(Class<?> type) {

      Predicate<Method> predicate = Predicates.builder(Method::isAccessible)
              .and(method -> Modifier.isStatic(method.getModifiers()))
              .and(method -> type.isAssignableFrom(method.getReturnType()))
              .and(USABLE_STATIC_FACTORY_METHOD_PARAMETERS_PREDICATE)
              .build();

      for (Method method : type.getDeclaredMethods()) {
        if (predicate.test(method)) {
          return true;
        }
      }
      return false;
    }

    @Override
    public String toString() {

      return "Valid Config Entity Type Check (has no arg constructor and annotated with ConfigEntity)";
    }
  }

  private class ValidDeserializeCustomizableTypePredicate implements Predicate<Class<?>> {

    @Override
    public boolean test(Class<?> aClass) {

      return ! (aClass.isPrimitive() || fixedDeserializers.containsKey(aClass));
    }
  }
}
