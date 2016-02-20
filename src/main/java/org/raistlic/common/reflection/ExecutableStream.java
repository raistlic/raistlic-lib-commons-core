package org.raistlic.common.reflection;

import org.raistlic.common.precondition.Precondition;
import org.raistlic.common.util.CustomStream;
import org.raistlic.common.util.CustomStreamAdapter;

import java.lang.annotation.Annotation;
import java.lang.reflect.Executable;
import java.lang.reflect.Parameter;
import java.util.Arrays;
import java.util.function.Predicate;
import java.util.stream.Stream;

/**
 * @author Lei Chen (2016-02-04)
 */
public class ExecutableStream<E extends Executable, ES extends ExecutableStream<E, ES>>
        extends CustomStreamAdapter<E, ES> implements CustomStream<E, ES> {

  ExecutableStream(Stream<E> originalStream) {

    super(originalStream);
  }

  public ES staticOnes() {

    return filter(ReflectionPredicates.executableIsStatic());
  }

  public ES publicOnes() {

    return filter(ReflectionPredicates.executableIsPublic());
  }

  public ES protectedOnes() {

    return filter(ReflectionPredicates.executableIsProtected());
  }

  public ES publicAndProtectedOnes() {

    return filter(ReflectionPredicates.executableIsPublicOrProtected());
  }

  public ES privateOnes() {

    return filter(ReflectionPredicates.executableIsPrivate());
  }

  public ES finalOnes() {

    return filter(ReflectionPredicates.executableIsFinal());
  }

  public ES abstractOnes() {

    return filter(ReflectionPredicates.executableIsAbstract());
  }

  public ES nativeOnes() {

    return filter(ReflectionPredicates.executableIsNative());
  }

  public ES hasParameterCount(int parameterCount) {

    return filter(ReflectionPredicates.executableWithParameterCount(parameterCount));
  }

  public ES hasParameterTypes(Class<?>... parameterTypes) {

    Precondition.param(parameterTypes, "parameterTypes").notNull();
    return filter(executable -> Arrays.equals(executable.getParameterTypes(), parameterTypes));
  }

  public ES hasAnyParameterMatches(Predicate<? super Parameter> predicate) {

    Precondition.param(predicate, "predicate").notNull();

    return filter(executable -> {
      for (Parameter parameter : executable.getParameters()) {
        if (predicate.test(parameter)) {
          return true;
        }
      }
      return false;
    });
  }

  public ES hasAllParametersMatch(Predicate<? super Parameter> predicate) {

    Precondition.param(predicate, "predicate").notNull();

    return filter(executable -> {
      for (Parameter parameter : executable.getParameters()) {
        if (!predicate.test(parameter)) {
          return false;
        }
      }
      return true;
    });
  }

  public ES annotatedWith(Class<? extends Annotation> annotationType) {

    Precondition.param(annotationType, "annotationType").notNull();
    return filter(executable -> executable.getAnnotation(annotationType) != null);
  }

  public <A extends Annotation> ES annotatedWith(Class<A> annotationType,
                                                           Predicate<? super A> annotationPredicate) {

    Precondition.param(annotationType, "annotationType").notNull();
    Precondition.param(annotationPredicate, "annotationPredicate").notNull();

    return filter(executable -> {
      A annotation = executable.getAnnotation(annotationType);
      return annotation != null && annotationPredicate.test(annotation);
    });
  }
}
