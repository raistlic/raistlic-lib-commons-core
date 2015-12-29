package org.raistlic.common.reflection;

import org.raistlic.common.precondition.Precondition;

import java.lang.annotation.Annotation;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.lang.reflect.Type;
import java.util.HashMap;
import java.util.Map;

/**
 * @author lei.c (2015-12-21)
 */
public class Reflections {

  public static <A extends Annotation> A getAnnotation(Type type, Class<A> annotationType) {

    if (type instanceof Class<?>) {

      A[] annotations = ((Class<?>) type).getAnnotationsByType(annotationType);
      if (annotations.length > 0) {
        return annotations[0];
      }
    }
    return null;
  }

  public static <A extends Annotation> A getAnnotation(Constructor<?> constructor, Class<A> annotationType) {

    Precondition.param(constructor, "constructor").notNull();
    Precondition.param(annotationType, "annotationType").notNull();

    for (Annotation annotation : constructor.getAnnotations()) {
      if (annotationType.isInstance(annotation)) {
        return annotationType.cast(annotation);
      }
    }
    return null;
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
