package org.raistlic.common.reflection;

import org.raistlic.common.precondition.Precondition;

import java.lang.annotation.Annotation;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

/**
 * @author lei.c (2015-12-21)
 */
public class Reflections {

  public static <A extends Annotation> A getAnnotation(Class<?> targetType, Class<A> annotationType) {

    Precondition.param(targetType, "targetType").notNull();
    Precondition.param(annotationType, "annotationType").notNull();

    List<Class<?>> stack = new ArrayList<>();
    stack.add(targetType);
    return getAnnotationRecursive(stack, annotationType);
  }

  private static <A extends Annotation> A getAnnotationRecursive(List<Class<?>> stack, Class<A> annotationType) {

    if (stack.isEmpty()) {
      return null;
    }

    Class<?> next = stack.remove(stack.size() - 1);
    A annotation = next.getAnnotation(annotationType);
    if (annotation != null) {
      return annotation;
    }

    stack.addAll(Arrays.asList(next.getInterfaces()));
    Class<?> superType = next.getSuperclass();
    if (superType != Object.class) {
      stack.add(superType);
    }
    return getAnnotationRecursive(stack, annotationType);
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

  public static <A extends Annotation> Map<Field, A> getAnnotatedFields(
          Class<?> type, Class<A> annotationType, boolean includeStaticFields) {

    Precondition.param(type, "type").notNull();
    Precondition.param(annotationType, "annotationType").notNull();

    Map<Field, A> map = new HashMap<>();
    for (Field field : type.getDeclaredFields()) {

      if ( (!includeStaticFields) && Modifier.isStatic(field.getModifiers()) ) {
        continue;
      }

      try {
        A annotation = field.getAnnotation(annotationType);
        if (annotation != null) {
          map.put(field, annotation);
        }
      } catch (Exception ex) {
        // TODO log error
      }
    }
    return map;
  }
}
