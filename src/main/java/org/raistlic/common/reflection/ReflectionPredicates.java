package org.raistlic.common.reflection;

import org.raistlic.common.precondition.Precondition;
import org.raistlic.common.predicate.Predicates;

import java.lang.annotation.Annotation;
import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Executable;
import java.lang.reflect.Modifier;
import java.util.function.Predicate;

/**
 * @author Lei Chen (2016-02-04)
 */
public final class ReflectionPredicates {

  // Executable ------------------------------------------------------------------------------------

  public static Predicate<Executable> executableIsStatic() {

    return EXECUTABLE_IS_STATIC;
  }

  public static Predicate<Executable> executableIsPublic() {

    return EXECUTABLE_IS_PUBLIC;
  }

  public static Predicate<Executable> executableIsProtected() {

    return EXECUTABLE_IS_PROTECTED;
  }

  public static Predicate<Executable> executableIsPublicOrProtected (){

    return EXECUTABLE_IS_PUBLIC_OR_PROTECTED;
  }

  public static Predicate<Executable> executableIsPrivate() {

    return EXECUTABLE_IS_PRIVATE;
  }

  public static Predicate<Executable> executableIsPrivateOrPackagePrivate() {

    return EXECUTABLE_IS_PRIVATE_OR_PACKAGE;
  }

  public static Predicate<Executable> executableIsFinal() {

    return EXECUTABLE_IS_FINAL;
  }

  public static Predicate<Executable> executableIsNative() {

    return EXECUTABLE_IS_NATIVE;
  }

  public static Predicate<Executable> executableIsAbstract() {

    return EXECUTABLE_IS_ABSTRACT;
  }

  public static Predicate<Executable> executableWithParameterCount(int parametersCount) {

    Precondition.param(parametersCount, "parametersCount").noLessThan(0);

    if (parametersCount == 0) {
      return ExecutableWithParametersCountPredicate.WITH_ZERO;
    }
    if (parametersCount == 1) {
      return ExecutableWithParametersCountPredicate.WITH_ONE;
    }
    return new ExecutableWithParametersCountPredicate(parametersCount);
  }

  public static Predicate<Executable> executableWithNoParameter() {

    return executableWithParameterCount(0);
  }

  public static Predicate<Executable> executableWithOneParameter() {

    return executableWithParameterCount(1);
  }

  private static final Predicate<Executable> EXECUTABLE_IS_STATIC =
          executable -> Modifier.isStatic(executable.getModifiers());

  private static final Predicate<Executable> EXECUTABLE_IS_PUBLIC =
          executable -> Modifier.isPublic(executable.getModifiers());

  private static final Predicate<Executable> EXECUTABLE_IS_PROTECTED =
          executable -> Modifier.isProtected(executable.getModifiers());

  private static final Predicate<Executable> EXECUTABLE_IS_PUBLIC_OR_PROTECTED =
          Predicates.or(EXECUTABLE_IS_PUBLIC, EXECUTABLE_IS_PROTECTED);

  private static final Predicate<Executable> EXECUTABLE_IS_PRIVATE =
          executable -> Modifier.isPrivate(executable.getModifiers());

  private static final Predicate<Executable> EXECUTABLE_IS_PRIVATE_OR_PACKAGE =
          Predicates.not(EXECUTABLE_IS_PUBLIC_OR_PROTECTED);

  private static final Predicate<Executable> EXECUTABLE_IS_FINAL =
          executable -> Modifier.isFinal(executable.getModifiers());

  private static final Predicate<Executable> EXECUTABLE_IS_NATIVE =
          executable -> Modifier.isNative(executable.getModifiers());

  private static final Predicate<Executable> EXECUTABLE_IS_ABSTRACT =
          executable -> Modifier.isAbstract(executable.getModifiers());

  private static final class ExecutableWithParametersCountPredicate implements Predicate<Executable> {

    private static final ExecutableWithParametersCountPredicate WITH_ZERO = new ExecutableWithParametersCountPredicate(0);
    private static final ExecutableWithParametersCountPredicate WITH_ONE = new ExecutableWithParametersCountPredicate(1);

    private final int count;

    private ExecutableWithParametersCountPredicate(int count) {

      this.count = count;
    }

    @Override
    public boolean test(Executable executable) {

      return executable != null && executable.getParameterCount() == count;
    }
  }

  // AnnotatedElement ------------------------------------------------------------------------------

  public static Predicate<AnnotatedElement> elementAnnotatedWith(Class<? extends Annotation> annotationType) {

    Precondition.param(annotationType, "annotationType").notNull();
    return new AnnotatedElementHasAnnotationPredicate(annotationType);
  }

  private static final class AnnotatedElementHasAnnotationPredicate implements Predicate<AnnotatedElement> {

    private final Class<? extends Annotation> annotationType;

    private AnnotatedElementHasAnnotationPredicate(Class<? extends Annotation> annotationType) {

      this.annotationType = annotationType;
    }

    @Override
    public boolean test(AnnotatedElement annotatedElement) {

      return annotatedElement != null && annotatedElement.getAnnotation(annotationType) != null;
    }
  }
}
