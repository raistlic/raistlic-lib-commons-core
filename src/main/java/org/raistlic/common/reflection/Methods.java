package org.raistlic.common.reflection;

import org.raistlic.common.precondition.Precondition;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.util.function.Predicate;

/**
 * @author lei.c (2015-12-22)
 */
public class Methods {

  public static Predicate<Method> predicateParametersAnnotatedWith(Class<? extends Annotation> annotationType) {

    Precondition.param(annotationType, "annotationType").notNull();

    return new MethodParametersAnnotatedWith(annotationType);
  }

  private static final class MethodParametersAnnotatedWith implements Predicate<Method> {

    private final Class<? extends Annotation> annotationType;

    private MethodParametersAnnotatedWith(Class<? extends Annotation> annotationType) {

      this.annotationType = annotationType;
    }

    @Override
    public boolean test(Method method) {

      if (method == null) {
        return false;
      }
      for (Parameter parameter : method.getParameters()) {
        if (!parameter.isAnnotationPresent(annotationType)) {
          return false;
        }
      }
      return true;
    }
  }

  public static Predicate<Method> predicateWithParameterCount(int parametersCount) {

    Precondition.param(parametersCount, "parametersCount").noLessThan(0);

    if (parametersCount == 0) {
      return MethodWithParametersCount.WITH_ZERO;
    }
    if (parametersCount == 1) {
      return MethodWithParametersCount.WITH_ONE;
    }
    return new MethodWithParametersCount(parametersCount);
  }

  public static Predicate<Method> predicateWithNoParameter() {

    return predicateWithParameterCount(0);
  }

  public static Predicate<Method> predicateWithOneParameter() {

    return predicateWithParameterCount(1);
  }

  private static final class MethodWithParametersCount implements Predicate<Method> {

    private static final Predicate<Method> WITH_ZERO = new MethodWithParametersCount(0);
    private static final Predicate<Method> WITH_ONE = new MethodWithParametersCount(1);

    private final int count;

    private MethodWithParametersCount(int count) {

      this.count = count;
    }

    @Override
    public boolean test(Method method) {

      return method != null && method.getParameterCount() == count;
    }
  }
}
