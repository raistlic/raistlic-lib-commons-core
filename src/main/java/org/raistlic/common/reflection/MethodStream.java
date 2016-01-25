package org.raistlic.common.reflection;

import org.raistlic.common.precondition.Precondition;
import org.raistlic.common.util.CustomStream;
import org.raistlic.common.util.CustomStreamAdapter;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Parameter;
import java.util.Arrays;
import java.util.function.Predicate;
import java.util.stream.Stream;

/**
 * @author Lei Chen (2016-01-25)
 */
public final class MethodStream extends CustomStreamAdapter<Method, MethodStream>
        implements CustomStream<Method, MethodStream> {

  MethodStream(Stream<Method> originalStream) {

    super(originalStream);
  }

  public MethodStream staticOnes() {

    return filter(PREDICATE_IS_STATIC);
  }

  public MethodStream returnsVoid() {

    return hasReturnType(void.class);
  }

  public MethodStream hasReturnType(Class<?> returnType) {

    Precondition.param(returnType, "returnType").notNull();
    return filter(m -> m.getReturnType() == returnType);
  }

  public MethodStream hasParameterCount(int parameterCount) {

    Precondition.param(parameterCount, "parameterCount").noLessThan(0);
    return filter(m -> m.getParameterCount() == parameterCount);
  }

  public MethodStream hasParameterTypes(Class<?>... parameterTypes) {

    Precondition.param(parameterTypes, "parameterTypes").notNull();
    return filter(m -> Arrays.equals(m.getParameterTypes(), parameterTypes));
  }

  public MethodStream hasAnyParameterMatches(Predicate<? super Parameter> predicate) {

    Precondition.param(predicate, "predicate").notNull();

    return filter(m -> {
      for (Parameter parameter : m.getParameters()) {
        if (predicate.test(parameter)) {
          return true;
        }
      }
      return false;
    });
  }

  public MethodStream hasAllParametersMatch(Predicate<? super Parameter> predicate) {

    Precondition.param(predicate, "predicate").notNull();

    return filter(m -> {
      for (Parameter parameter : m.getParameters()) {
        if (!predicate.test(parameter)) {
          return false;
        }
      }
      return true;
    });
  }

  public MethodStream annotatedWith(Class<? extends Annotation> annotationType) {

    Precondition.param(annotationType, "annotationType").notNull();
    return filter(m -> m.getAnnotation(annotationType) != null);
  }

  public <A extends Annotation> MethodStream annotatedWith(Class<A> annotationType,
                                                           Predicate<? super A> annotationPredicate) {

    Precondition.param(annotationType, "annotationType").notNull();
    Precondition.param(annotationPredicate, "annotationPredicate").notNull();

    return filter(m -> {
      A annotation = m.getAnnotation(annotationType);
      return annotation != null && annotationPredicate.test(annotation);
    });
  }

  private static final Predicate<Method> PREDICATE_IS_STATIC = m -> Modifier.isStatic(m.getModifiers());
}
