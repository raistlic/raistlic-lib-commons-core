package org.raistlic.common.reflection;

import org.raistlic.common.precondition.Precondition;

import java.lang.annotation.Annotation;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Type;
import java.util.Map;

/**
 * @author lei.c (2015-12-21)
 */
public class Reflections {

  public static <A extends Annotation> A getAnnotation(Type type, Class<A> annotationType) {

    throw new UnsupportedOperationException();
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
          Type type, Class<A> annotationType, boolean includeStaticFields) {

    throw new UnsupportedOperationException();
  }
}
