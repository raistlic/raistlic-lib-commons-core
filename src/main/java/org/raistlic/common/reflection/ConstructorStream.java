package org.raistlic.common.reflection;

import org.raistlic.common.precondition.Precondition;
import org.raistlic.common.util.CustomStream;

import java.lang.annotation.Annotation;
import java.lang.reflect.Constructor;
import java.lang.reflect.Parameter;
import java.util.Collection;
import java.util.function.Predicate;
import java.util.stream.Stream;

/**
 * @author lei.c (2015-12-22)
 */
public class ConstructorStream<E> extends ExecutableStream<Constructor<E>, ConstructorStream<E>>
    implements CustomStream<Constructor<E>, ConstructorStream<E>> {

  public static <E> ConstructorStream<E> of(Stream<Constructor<E>> constructorStream) {

    Precondition.param(constructorStream, "constructorStream").isNotNull();
    return new ConstructorStream<>(constructorStream);
  }

  public static <E> ConstructorStream<E> of(Collection<Constructor<E>> constructorCollection) {

    Precondition.param(constructorCollection, "constructorCollection").isNotNull();
    return new ConstructorStream<>(constructorCollection.stream());
  }

  ConstructorStream(Stream<Constructor<E>> originalStream) {
    super(originalStream);
  }

  @Deprecated
  public static Predicate<Constructor<?>> predicateParametersAnnotatedWith(Class<? extends Annotation> annotationType) {

    Precondition.param(annotationType, "annotationType").isNotNull();

    return new ConstructorParametersAnnotatedWith(annotationType);
  }

  @Deprecated
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
