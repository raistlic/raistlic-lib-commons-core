package org.raistlic.common.reflection;

import org.raistlic.common.precondition.Precondition;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * @author lei.c (2015-12-21)
 */
public class Reflections {

  public static <A extends Annotation> A getAnnotation(Class<?> targetType,
                                                       Class<A> annotationType,
                                                       boolean includeSuperTypes) {

    Precondition.param(targetType, "targetType").isNotNull();
    Precondition.param(annotationType, "annotationType").isNotNull();

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

  /**
   * Checks the {@code targetMethod} and returns the annotation of the specified {@code annotationType} , if present;
   * alternatively check if the {@code targetMethod} is an override of some super type method, and checks the annotations
   * on the overridden method if any, when ({@code includeOverriddenMethods} is {@code true} (not applicable to static
   * methods). Note that overridden methods with generic type parameters are not supported/checked.
   *
   * @param targetMethod the method to search annotations on.
   * @param annotationType the type of annotation to search.
   * @param includeOverriddenMethods {@code true} if check possible overridden methods' annotations as well, when annotation
   *                                             is not found on the {@code targetMethod} itself.
   * @param <A> the actual annotation type.
   * @return the annotation instance found, or {@code null} if none found.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code targetMethod} or
   *         {@code annotationType} is {@code null}.
   */
  public static <A extends Annotation> A getAnnotation(Method targetMethod,
                                                       Class<A> annotationType,
                                                       boolean includeOverriddenMethods) {

    Precondition.param(targetMethod, "targetMethod").isNotNull();
    Precondition.param(annotationType, "annotationType").isNotNull();

    if (includeOverriddenMethods) {
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
}
