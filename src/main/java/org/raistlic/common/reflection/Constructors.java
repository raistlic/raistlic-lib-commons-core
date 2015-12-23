package org.raistlic.common.reflection;

import org.raistlic.common.precondition.Precondition;
import org.raistlic.common.predicate.PredicateOrWrapper;

import java.lang.annotation.Annotation;
import java.lang.reflect.Constructor;
import java.lang.reflect.Parameter;
import java.util.function.Predicate;

/**
 * @author lei.c (2015-12-22)
 */
public class Constructors {

  public static Predicate<Constructor<?>> predicateWithParametersCount(int parametersCount) {

    Precondition.param(parametersCount, "parametersCount").noLessThan(0);

    if (parametersCount == 0) {
      return ConstructorWithParametersCount.WITH_ZERO;
    }
    if (parametersCount == 1) {
      return ConstructorWithParametersCount.WITH_ONE;
    }
    return new ConstructorWithParametersCount(parametersCount);
  }

  public static Predicate<Constructor<?>> predicateWithNoParameter() {

    return predicateWithParametersCount(0);
  }

  public static Predicate<Constructor<?>> predicateWithOneParameter() {

    return predicateWithParametersCount(1);
  }

  private static final class ConstructorWithParametersCount implements Predicate<Constructor<?>> {

    private static final ConstructorWithParametersCount WITH_ZERO = new ConstructorWithParametersCount(0);
    private static final ConstructorWithParametersCount WITH_ONE = new ConstructorWithParametersCount(1);

    private final int count;

    private ConstructorWithParametersCount(int count) {

      this.count = count;
    }

    @Override
    public boolean test(Constructor<?> constructor) {

      return constructor != null && constructor.getParameterCount() == count;
    }
  }

  public static Predicate<Constructor<?>> predicateParametersAnnotatedWith(Class<? extends Annotation> annotationType) {

    Precondition.param(annotationType, "annotationType").notNull();

    return new ConstructorParametersAnnotatedWith(annotationType);
  }

  private static final class ConstructorParametersAnnotatedWith implements Predicate<Constructor<?>> {

    private final Class<? extends Annotation> annotationType;

    private ConstructorParametersAnnotatedWith(Class<? extends Annotation> annotationType) {

      this.annotationType = annotationType;
    }

    @Override
    public boolean test(Constructor<?> constructor) {

      if (constructor == null) {
        return false;
      }
      for (Parameter parameter : constructor.getParameters()) {
        if (!parameter.isAnnotationPresent(annotationType)) {
          return false;
        }
      }
      return true;
    }
  }

  private Constructors() { }
}
