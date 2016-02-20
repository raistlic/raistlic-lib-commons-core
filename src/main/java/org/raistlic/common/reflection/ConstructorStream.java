package org.raistlic.common.reflection;

import org.raistlic.common.precondition.Precondition;
import org.raistlic.common.util.CustomStream;

import java.lang.annotation.Annotation;
import java.lang.reflect.Constructor;
import java.lang.reflect.Parameter;
import java.util.function.Predicate;
import java.util.stream.Stream;

/**
 * @author lei.c (2015-12-22)
 */
public class ConstructorStream<E> extends ExecutableStream<Constructor<E>, ConstructorStream<E>>
    implements CustomStream<Constructor<E>, ConstructorStream<E>> {

  ConstructorStream(Stream<Constructor<E>> originalStream) {
    super(originalStream);
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
}
