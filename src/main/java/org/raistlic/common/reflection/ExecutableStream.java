package org.raistlic.common.reflection;

import org.raistlic.common.util.CustomStream;

import java.lang.annotation.Annotation;
import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Executable;
import java.lang.reflect.Parameter;
import java.util.function.Predicate;
import java.util.stream.Stream;

/**
 * @author Lei Chen (2016-02-04)
 */
public abstract class ExecutableStream<E extends Executable, ES extends ExecutableStream<E, ES>>
        extends MemberStream<E, ES> implements CustomStream<E, ES> {

  ExecutableStream(Stream<E> originalStream) {

    super(originalStream);
  }

  public ES hasParameterCount(int parameterCount) {

    return filter(ReflectionPredicates.executableWithParameterCount(parameterCount));
  }

  public ES hasNoParameter() {

    return filter(ReflectionPredicates.executableWithNoParameter());
  }

  public ES hasOneParameter() {

    return filter(ReflectionPredicates.executableWithOneParameter());
  }

  public ES hasParameterTypes(Class<?>... parameterTypes) {

    Predicate<Executable> predicate = ReflectionPredicates.executableWithParameterTypes(parameterTypes);
    return filter(predicate);
  }

  public ES hasAnyParameterMatches(Predicate<? super Parameter> parameterPredicate) {

    Predicate<Executable> predicate =
            ReflectionPredicates.executableWithAnyParameterMatches(parameterPredicate);
    return filter(predicate);
  }

  public ES hasAllParametersMatch(Predicate<? super Parameter> parameterPredicate) {

    Predicate<Executable> predicate = ReflectionPredicates.executableWithAllParametersMatch(parameterPredicate);
    return filter(predicate);
  }

  public ES annotatedWith(Class<? extends Annotation> annotationType) {

    Predicate<AnnotatedElement> predicate = ReflectionPredicates.elementAnnotatedWith(annotationType);
    return filter(predicate);
  }

  public <A extends Annotation> ES annotatedWith(Class<A> annotationType,
                                                 Predicate<? super A> annotationPredicate) {

    Predicate<AnnotatedElement> predicate =
            ReflectionPredicates.elementAnnotatedWith(annotationType, annotationPredicate);
    return filter(predicate);
  }
}
