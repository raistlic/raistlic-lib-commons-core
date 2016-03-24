package org.raistlic.common.reflection;

import org.raistlic.common.precondition.Precondition;
import org.raistlic.common.predicate.Predicates;

import java.lang.annotation.Annotation;
import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Executable;
import java.lang.reflect.Member;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Parameter;
import java.util.Arrays;
import java.util.function.Predicate;

/**
 * The class holds a collections of static factory methods that export different types of
 * {@link Predicate} implementations for the reflection utility types.
 *
 * @author Lei Chen (2016-02-04)
 */
public final class ReflectionPredicates {

  // Member ----------------------------------------------------------------------------------------

  /**
   * Returns the {@link Predicate} to test whether an {@link Member} instance is static.
   * @return the {@link Predicate} to test whether an {@link Member} instance is static.
   */
  public static Predicate<Member> memberIsStatic() {

    return MemberPredicates.IS_STATIC;
  }

  /**
   * Returns the {@link Predicate} to test whether an {@link Member} instance is not static.
   * @return the {@link Predicate} to test whether an {@link Member} instance is not static.
   */
  public static Predicate<Member> memberIsNotStatic() {

    return MemberPredicates.IS_NOT_STATIC;
  }

  /**
   * Returns the {@link Predicate} to test whether an {@link Member} instance is public.
   * @return the {@link Predicate} to test whether an {@link Member} instance is public.
   */
  public static Predicate<Member> memberIsPublic() {

    return MemberPredicates.IS_PUBLIC;
  }

  /**
   * Returns the {@link Predicate} to test whether an {@link Member} instance is protected.
   * @return the {@link Predicate} to test whether an {@link Member} instance is protected.
   */
  public static Predicate<Member> memberIsProtected() {

    return MemberPredicates.IS_PROTECTED;
  }

  /**
   * Returns the {@link Predicate} to test whether an {@link Member} instance is public or protected.
   * @return the {@link Predicate} to test whether an {@link Member} instance is public or protected.
   */
  public static Predicate<Member> memberIsPublicOrProtected(){

    return MemberPredicates.IS_PUBLIC_OR_PROTECTED;
  }

  /**
   * Returns the {@link Predicate} to test whether an {@link Member} instance is private.
   * @return the {@link Predicate} to test whether an {@link Member} instance is private.
   */
  public static Predicate<Member> memberIsPrivate() {

    return MemberPredicates.IS_PRIVATE;
  }

  public static Predicate<Member> memberIsPackagePrivate() {

    return MemberPredicates.IS_PACKAGE_PRIVATE;
  }

  public static Predicate<Member> memberIsPrivateOrPackagePrivate() {

    return MemberPredicates.IS_PRIVATE_OR_PACKAGE_PRIVATE;
  }

  public static Predicate<Member> memberIsFinal() {

    return MemberPredicates.IS_FINAL;
  }

  public static Predicate<Member> memberIsNative() {

    return MemberPredicates.IS_NATIVE;
  }

  public static Predicate<Member> memberIsAbstract() {

    return MemberPredicates.IS_ABSTRACT;
  }

  private enum MemberPredicates implements Predicate<Member> {

    IS_STATIC ("member is static") {
      @Override
      public boolean test(Member member) {
        return Modifier.isStatic(member.getModifiers());
      }
    },
    IS_NOT_STATIC ("member is not static") {
      @Override
      public boolean test(Member member) {
        return !Modifier.isStatic(member.getModifiers());
      }
    },
    IS_PUBLIC ("member is public") {
      @Override
      public boolean test(Member member) {
        return Modifier.isPublic(member.getModifiers());
      }
    },
    IS_PROTECTED ("member is protected") {
      @Override
      public boolean test(Member member) {
        return Modifier.isProtected(member.getModifiers());
      }
    },
    IS_PUBLIC_OR_PROTECTED ("member is public or protected") {
      @Override
      public boolean test(Member member) {
        int modifiers = member.getModifiers();
        return Modifier.isPublic(modifiers) || Modifier.isProtected(modifiers);
      }
    },
    IS_PRIVATE ("member is private") {
      @Override
      public boolean test(Member member) {
        return Modifier.isPrivate(member.getModifiers());
      }
    },
    IS_PACKAGE_PRIVATE ("member is package private") {
      @Override
      public boolean test(Member member) {
        return (member.getModifiers() & (Modifier.PUBLIC | Modifier.PROTECTED | Modifier.PRIVATE)) == 0;
      }
    },
    IS_PRIVATE_OR_PACKAGE_PRIVATE ("member is private or package private") {
      @Override
      public boolean test(Member member) {
        int modifiers = member.getModifiers();
        return Modifier.isPrivate(modifiers) || (modifiers & (Modifier.PUBLIC | Modifier.PROTECTED | Modifier.PRIVATE)) == 0;
      }
    },
    IS_FINAL ("member is final") {
      @Override
      public boolean test(Member member) {
        return Modifier.isFinal(member.getModifiers());
      }
    },
    IS_NATIVE ("member is native") {
      @Override
      public boolean test(Member member) {
        return Modifier.isNative(member.getModifiers());
      }
    },
    IS_ABSTRACT ("member is abstract") {
      @Override
      public boolean test(Member member) {
        return Modifier.isAbstract(member.getModifiers());
      }
    };

    private final String description;

    MemberPredicates(String description) {

      this.description = description;
    }

    @Override
    public String toString() {

      return description;
    }
  }

  // Executable ------------------------------------------------------------------------------------

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

  public static Predicate<Executable> executableWithParameterTypes(Class<?>... parameterTypes) {

    return new ExecutableWithParameterTypes(parameterTypes);
  }

  public static Predicate<Executable> executableWithAllParametersMatch(
          Predicate<? super Parameter> parameterPredicate) {

    Precondition.param(parameterPredicate, "parameterPredicate").isNotNull();
    return new ExecutableWithAllParametersMatchPredicate(parameterPredicate);
  }

  public static Predicate<Executable> executableWithAnyParameterMatches(
          Predicate<? super Parameter> parameterPredicate) {

    Precondition.param(parameterPredicate, "parameterPredicate").isNotNull();
    return new ExecutableWithAnyParameterMatchesPredicate(parameterPredicate);
  }

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

  private static final class ExecutableWithParameterTypes implements Predicate<Executable> {

    private final Class<?>[] parameterTypes;

    private ExecutableWithParameterTypes(Class<?>[] parameterTypes) {

      this.parameterTypes = parameterTypes;
    }

    @Override
    public boolean test(Executable executable) {

      if (executable == null) {
        return false;
      }
      return Arrays.equals(executable.getParameterTypes(), parameterTypes);
    }
  }

  private static final class ExecutableWithAllParametersMatchPredicate implements Predicate<Executable> {

    private final Predicate<? super Parameter> parameterPredicate;

    private ExecutableWithAllParametersMatchPredicate(Predicate<? super Parameter> parameterPredicate) {

      this.parameterPredicate = parameterPredicate;
    }

    @Override
    public boolean test(Executable executable) {

      for (Parameter parameter : executable.getParameters()) {
        if (!parameterPredicate.test(parameter)) {
          return false;
        }
      }
      return true;
    }
  }

  private static final class ExecutableWithAnyParameterMatchesPredicate implements Predicate<Executable> {

    private final Predicate<? super Parameter> parameterPredicate;

    private ExecutableWithAnyParameterMatchesPredicate(Predicate<? super Parameter> parameterPredicate) {

      this.parameterPredicate = parameterPredicate;
    }

    @Override
    public boolean test(Executable executable) {

      for (Parameter parameter : executable.getParameters()) {
        if (parameterPredicate.test(parameter)) {
          return true;
        }
      }
      return false;
    }
  }

  // AnnotatedElement ------------------------------------------------------------------------------

  public static Predicate<AnnotatedElement> elementAnnotatedWith(Class<? extends Annotation> annotationType) {

    Precondition.param(annotationType, "annotationType").isNotNull();
    return new AnnotatedElementAnnotatedWithTypePredicate(annotationType);
  }

  public static <A extends Annotation> Predicate<AnnotatedElement> elementAnnotatedWith(
          Class<A> annotationType, Predicate<? super A> annotationPredicate) {

    Precondition.param(annotationType, "annotationType").isNotNull();
    Precondition.param(annotationPredicate, "annotationPredicate").isNotNull();
    return new AnnotatedElementAnnotatedWithSpecificPredicate<>(annotationType, annotationPredicate);
  }

  private static final class AnnotatedElementAnnotatedWithTypePredicate implements Predicate<AnnotatedElement> {

    private final Class<? extends Annotation> annotationType;

    private AnnotatedElementAnnotatedWithTypePredicate(Class<? extends Annotation> annotationType) {

      this.annotationType = annotationType;
    }

    @Override
    public boolean test(AnnotatedElement annotatedElement) {

      return annotatedElement != null && annotatedElement.getAnnotation(annotationType) != null;
    }
  }

  private static final class AnnotatedElementAnnotatedWithSpecificPredicate<A extends Annotation>
          implements Predicate<AnnotatedElement> {

    private final Class<A> annotationType;

    private final Predicate<? super A> annotationPredicate;

    private AnnotatedElementAnnotatedWithSpecificPredicate(Class<A> annotationType,
                                                           Predicate<? super A> annotationPredicate) {

      this.annotationType = annotationType;
      this.annotationPredicate = annotationPredicate;
    }

    @Override
    public boolean test(AnnotatedElement annotatedElement) {

      if (annotatedElement == null) {
        return false;
      }
      A annotation = annotatedElement.getAnnotation(annotationType);
      return (annotation != null) && annotationPredicate.test(annotation);
    }
  }

  // Method ----------------------------------------------------------------------------------------

  public static Predicate<Method> methodNameMatches(Predicate<? super String> namePredicate) {

    Precondition.param(namePredicate, "namePredicate").isNotNull();
    return new MethodNamePredicate(namePredicate);
  }

  public static Predicate<Method> methodHasName(String name) {

    Precondition.param(name).notNullOrEmpty();
    return methodNameMatches(name::equals);
  }

  public static Predicate<Method> methodOverrides(Method methodOverridden) {

    Precondition.param(methodOverridden, "methodOverridden").isNotNull();
    return new MethodOverridesAnotherPredicate(methodOverridden);
  }

  private static class MethodNamePredicate implements Predicate<Method> {

    private final Predicate<? super String> namePredicate;

    private MethodNamePredicate(Predicate<? super String> namePredicate) {

      this.namePredicate = namePredicate;
    }

    @Override
    public boolean test(Method method) {

      return namePredicate.test(method.getName());
    }
  }

  /**
   * The method returns a {@link Predicate} to test whether a method is overriden by the specified
   * {@code overridingMethod} .
   *
   * @param overridingMethod the overriding method used to test whether a method is overridden by it,
   *                         cannot be {@code null}.
   * @return the predicate instance created.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code overridingMethod}
   *         is {@code null}.
   */
  public static Predicate<Method> methodOverriddenBy(Method overridingMethod) {

    Precondition.param(overridingMethod, "overridingMethod").isNotNull();
    return new MethodOverriddenByAnotherPredicate(overridingMethod);
  }

  private static class MethodOverridesAnotherPredicate implements Predicate<Method> {

    private final Method overriddenMethod;

    private Predicate<Class<?>> declaringClassPredicate;

    private MethodOverridesAnotherPredicate(Method overriddenMethod) {

      this.overriddenMethod = overriddenMethod;
    }

    private Predicate<Class<?>> getDeclaringClassPredicate() {

      if (declaringClassPredicate == null) {
        declaringClassPredicate = hasSuperType(overriddenMethod.getDeclaringClass());
      }
      return declaringClassPredicate;
    }

    @Override
    public boolean test(Method method) {

      if (!getDeclaringClassPredicate().test(method.getDeclaringClass())) {
        return false;
      }
      if (!overriddenMethod.getName().equals(method.getName())) {
        return false;
      }
      if (!overriddenMethod.getReturnType().isAssignableFrom(method.getReturnType())) {
        return false;
      }
      return true;
    }
  }

  private static class MethodOverriddenByAnotherPredicate implements Predicate<Method> {

    private final Method overridingMethod;

    private Predicate<Class<?>> declaringClassPredicate;

    private MethodOverriddenByAnotherPredicate(Method overridingMethod) {

      this.overridingMethod = overridingMethod;
    }

    private Predicate<Class<?>> getDeclaringClassPredicate() {

      if (declaringClassPredicate == null) {
        declaringClassPredicate = hasSubType(overridingMethod.getDeclaringClass());
      }
      return declaringClassPredicate;
    }

    @Override
    public boolean test(Method method) {

      if (method == null) {
        return false;
      }
      Class<?> candidateDeclaringClass = method.getDeclaringClass();
      if (!getDeclaringClassPredicate().test(candidateDeclaringClass)) {
        return false;
      }
      if (!method.getName().equals(overridingMethod.getName())) {
        return false;
      }

      return true;
    }
  }

  // Class -----------------------------------------------------------------------------------------

  /**
   * The method returns a {@link Predicate} instance to test whether a class has the specified
   * {@code superType} .
   *
   * @param superType the super type used to test classes, cannot be {@code null}.
   * @return the predicate instance created.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code superType} is
   *         {@code null}.
   */
  public static Predicate<Class<?>> hasSuperType(Class<?> superType) {

    Precondition.param(superType, "superType").isNotNull();
    return new ClassHasSuperTypePredicate(superType);
  }

  /**
   * The method returns a {@link Predicate} instance to test whether a class has the specified
   * {@code subType} .
   *
   * @param subType the sub type used to test classes, cannot be {@code null}.
   * @return the predicate instance created.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code subType} is
   *         {@code null}.
   */
  public static Predicate<Class<?>> hasSubType(Class<?> subType) {

    Precondition.param(subType, "subType").isNotNull();
    return new ClassHasSubTypePredicate(subType);
  }

  private static final class ClassHasSuperTypePredicate implements Predicate<Class<?>> {

    private final Class<?> superType;

    private ClassHasSuperTypePredicate(Class<?> superType) {

      this.superType = superType;
    }

    @Override
    public boolean test(Class<?> aClass) {

      if (aClass == null) {
        return false;
      }
      return (aClass != superType) && superType.isAssignableFrom(aClass);
    }
  }

  private static final class ClassHasSubTypePredicate implements Predicate<Class<?>> {

    private final Class<?> subType;

    private ClassHasSubTypePredicate(Class<?> subType) {

      this.subType = subType;
    }

    @Override
    public boolean test(Class<?> aClass) {

      if (aClass == null) {
        return false;
      }
      return (aClass != subType) && aClass.isAssignableFrom(subType);
    }
  }
}
