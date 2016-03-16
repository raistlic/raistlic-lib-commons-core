package org.raistlic.common.reflection;

import org.raistlic.common.precondition.Precondition;

import java.lang.annotation.Annotation;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

/**
 * @author lei.c (2015-12-21)
 */
public class Reflections {

  public static <A extends Annotation> A getAnnotation(Class<?> targetType,
                                                       Class<A> annotationType,
                                                       boolean includeSuperTypes) {

    Precondition.param(targetType, "targetType").notNull();
    Precondition.param(annotationType, "annotationType").notNull();

    if (includeSuperTypes) {
      for (Class<?> type : gatherAllSuperTypes(targetType)) {
        A annotation = type.getAnnotation(annotationType);
        if (annotation != null) {
          return annotation;
        }
      }
      return null;
    } else {
      return targetType.getAnnotation(annotationType);
    }
  }

  public static <A extends Annotation> A getAnnotation(Method targetMethod,
                                                       Class<A> annotationType,
                                                       boolean includeOverridenMethods) {

    Precondition.param(targetMethod, "targetMethod").notNull();
    Precondition.param(annotationType, "annotationType").notNull();

    if (includeOverridenMethods) {
      throw new UnsupportedOperationException();
    } else {
      return targetMethod.getAnnotation(annotationType);
    }
  }

  public static List<Class<?>> gatherAllSuperTypes(Class<?> targetType) {

    Set<Class<?>> buffer = new LinkedHashSet<>();
    gatherAllSuperTypesRecursive(buffer, targetType);
    return new ArrayList<>(buffer);
  }

  private static void gatherAllSuperTypesRecursive(Set<Class<?>> buffer, Class<?> type) {

    if (type == Object.class) {
      return;
    }
    buffer.add(type);
    gatherAllSuperTypesRecursive(buffer, type.getSuperclass());
    for (Class<?> interf : type.getInterfaces()) {
      gatherAllSuperTypesRecursive(buffer, interf);
    }
  }

  public static MethodStream methodStreamOf(Class<?> targetType) {

    Precondition.param(targetType, "targetType").notNull();

    Set<Method> methods = new HashSet<>();
    methods.addAll(Arrays.asList(targetType.getMethods()));
    methods.addAll(Arrays.asList(targetType.getDeclaredMethods()));
    return methodStreamOf(methods.stream());
  }

  public static MethodStream methodStreamOf(Iterable<Method> methods) {

    Precondition.param(methods, "methods").notNull();
    return methodStreamOf(StreamSupport.stream(methods.spliterator(), false));
  }

  public static MethodStream methodStreamOf(Stream<Method> methodStream) {

    Precondition.param(methodStream, "methodStream").notNull();
    return new MethodStream(methodStream);
  }

  @SuppressWarnings("unchecked")
  public static <E> ConstructorStream<E> constructorStreamOf(Class<E> targetType) {

    Precondition.param(targetType, "targetType").notNull();
    Stream<Constructor<E>> originalStream = Arrays.asList(targetType.getConstructors())
        .stream()
        .map(c -> (Constructor<E>) c);
    return new ConstructorStream<>(originalStream);
  }

  public static FieldStream fieldStreamOf(Class<?> targetType) {

    Precondition.param(targetType, "targetType").notNull();

    Set<Field> fields = new HashSet<>();
    fields.addAll(Arrays.asList(targetType.getFields()));
    fields.addAll(Arrays.asList(targetType.getDeclaredFields()));
    return fieldStreamOf(fields);
  }

  public static FieldStream fieldStreamOf(Iterable<Field> fields) {

    Precondition.param(fields, "fields").notNull();
    return fieldStreamOf(StreamSupport.stream(fields.spliterator(), false));
  }

  public static FieldStream fieldStreamOf(Stream<Field> fieldStream) {

    Precondition.param(fieldStream, "fieldStream").notNull();
    return new FieldStream(fieldStream);
  }
}
