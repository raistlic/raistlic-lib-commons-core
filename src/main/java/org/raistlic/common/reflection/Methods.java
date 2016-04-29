package org.raistlic.common.reflection;

import org.raistlic.common.precondition.Precondition;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;
import java.util.stream.Collectors;

/**
 * @author lei.c (2015-12-22)
 */
public class Methods {

  /**
   * The method finds all overridden methods by the specified {@code method}, in such an order that, overridden methods
   * declared in interfaces {@code >} the ones declared in super classes {@code >} the ones declared in super class'
   * interfaces, etc.
   *
   * The method does not support methods with generic parameter types, nor does it support finding overridden methods
   * with generic parameter types... yet. Simply because matching generic signatures at runtime using reflection is
   * so tedious, and the inheritance hierarchy has so many possible cases to handle, it (potentially) requires large
   * amount of work, crazy recursive logic and careful design.
   *
   * @param method the method to find overridden methods for, cannot be {@code null} or static.
   * @return the list of overridden methods found, or empty list if none found, never returns {@code null}.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code method} is {@code null} or static.
   * @throws UnsupportedOperationException if {@code method} has generic type parameters.
   */
  public static List<Method> findOverriddenMethods(Method method) {

    Precondition.param(method, "method").isNotNull();
    Precondition.param(method, "method").matches(ReflectionPredicates.memberIsNotStatic());
    Arrays.asList(method.getParameters()).forEach(parameter -> {
      if (parameter.getParameterizedType() != null) {
        throw new UnsupportedOperationException("Methods with generic type parameters are not supported.");
      }
    });

    String methodName = method.getName();
    Class<?>[] methodParameterTypes = method.getParameterTypes();
    Class<?> declaringClass = method.getDeclaringClass();
    return Types.findSuperTypes(declaringClass).stream()
        .flatMap(type -> ClassHelper.of(type).getDeclaredMethodsAsStream()
            .noneStaticOnes()
            .hasName(methodName)
            .hasParameterTypes(methodParameterTypes)
        )
        .collect(Collectors.toList());
  }

  public static Predicate<Method> predicateParametersAnnotatedWith(Class<? extends Annotation> annotationType) {

    Precondition.param(annotationType, "annotationType").isNotNull();

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

  final static class OverriddenMethodFinder implements Predicate<Method> {

    private final Method overridingMethod;

    private Map<Class<?>, ParameterizedType> ptMap; // possible parameterized types map

    private OverriddenMethodFinder(Method overridingMethod) {

      this.overridingMethod = overridingMethod;
    }

    private ParameterizedType getPossibleGenericType(Class<?> type) {

      if (ptMap == null) {
        ptMap = new HashMap<>();
        Class<?> declaringClass = overridingMethod.getDeclaringClass();
//        gatherSuperTypes(declaringClass, ptMap);
      }
      return ptMap.get(type);
    }
//
//    private Predicate<Method> getPredicateFor(Method method, Class<?> declaringClass) {
//
//    }
//
//    private Predicate<Method> getPredicateFor(Method method, ParameterizedType parameterizedType) {
//
//    }

    @Override
    public boolean test(Method method) {

      Class<?> declaringClass = method.getDeclaringClass();
      throw new UnsupportedOperationException();
    }

    Method getOverridenMethod(Type type) {

      Precondition.param(type, "type").isNotNull();
      throw new UnsupportedOperationException();

//      Type genericType = getPossibleGenericType(type);
//      if (genericType == null) {
//        return null;
//      }
//      if (searched.contains(genericType)) {
//        return found.get(genericType);
//      }
//
//      Predicate<Method> paramsPredicate = new OverriddenMethodParametersPredicateForRawType(overridingMethod);
//      if (genericType instanceof ParameterizedType) {
//        paramsPredicate = Predicates.or(
//                paramsPredicate,
//                new OverriddenMethodParametersPredicateForParameterizedType(overridingMethod, (ParameterizedType) genericType)
//        );
//      }
//      Class<?> ownerClass = classOf(type);
//      return Reflections.methodStreamOf(Arrays.asList(ownerClass.getDeclaredMethods()))
//              .noneStaticOnes()
//              .hasName(overridingMethod.getName())
//              .hasReturnTypeMatches(returnType -> returnType.isAssignableFrom(overridingMethod.getReturnType()))
//              .filter(paramsPredicate)
//              .findAny()
//              .orElse(null);
    }

    private static class OverriddenMethodParametersPredicateForParameterizedType implements Predicate<Method> {

      private final Parameter[] overridingParameters;

      private final ParameterizedType parameterizedType;

      private OverriddenMethodParametersPredicateForParameterizedType(
              Method overridingMethod, ParameterizedType parameterizedType) {

        this.overridingParameters = overridingMethod.getParameters();
        this.parameterizedType = parameterizedType;
      }

      @Override
      public boolean test(Method method) {

        throw new UnsupportedOperationException();
      }
    }

    private static class OverriddenMethodParametersPredicateForRawType implements Predicate<Method> {

      private final Parameter[] overridingParameters;

      private OverriddenMethodParametersPredicateForRawType(Method overridingMethod) {

        this.overridingParameters = overridingMethod.getParameters();
      }

      @Override
      public boolean test(Method method) {

        Parameter[] parameters = method.getParameters();
        if (parameters.length != overridingParameters.length) {
          return false;
        }
        for (int i = 0, len = parameters.length; i < len; i++) {
          Parameter overridingParameter = overridingParameters[i];
          Parameter parameter = parameters[i];
          if (overridingParameter.getType() != parameter.getType()) {
            return false;
          }
        }
        return true;
      }
    }

    private static void gatherParameterizedTypes(
            Class<?> targetClass, Map<Class<?>, ParameterizedType> map) {

      for (Type genericInterface : targetClass.getGenericInterfaces()) {
        if (genericInterface instanceof ParameterizedType) {
          ParameterizedType pt = (ParameterizedType) genericInterface;
          Class<?> rawType = (Class<?>) pt.getRawType();
          map.put(rawType, pt);
        }
      }

    }

    private static void gatherSuperTypes(Class<?> targetClass, Map<Type, Type> genericTypeMap) {

      for (Type genericInterface : targetClass.getGenericInterfaces()) {
        if (gatherAndRegisterIfNotYetGathered(genericTypeMap, genericInterface)) {
          gatherSuperTypes(classOf(genericInterface), genericTypeMap);
        }
      }
      Type genericSuperType = targetClass.getGenericSuperclass();
      if (genericSuperType != null && genericSuperType != Object.class &&
              gatherAndRegisterIfNotYetGathered(genericTypeMap, genericSuperType)) {
        gatherSuperTypes(classOf(genericSuperType), genericTypeMap);
      }
      for (Class<?> normalInterface : targetClass.getInterfaces()) {
        if (gatherAndRegisterIfNotYetGathered(genericTypeMap, normalInterface)) {
          gatherSuperTypes(normalInterface, genericTypeMap);
        }
      }
      Class<?> superType = targetClass.getSuperclass();
      if (superType != null && superType != Object.class &&
              gatherAndRegisterIfNotYetGathered(genericTypeMap, superType)) {
        gatherSuperTypes(superType, genericTypeMap);
      }
    }

    private static Class<?> classOf(Type type) {

      if (type instanceof ParameterizedType) {
        return (Class<?>) ((ParameterizedType) type).getRawType();
      } else {
        return (Class<?>) type;
      }
    }

    private static boolean gatherAndRegisterIfNotYetGathered(Map<Type, Type> genericTypeMap, Type type) {

      if (genericTypeMap.containsKey(type)) {
        return false;
      }
      if (type instanceof ParameterizedType) {
        ParameterizedType pt = (ParameterizedType) type;
        genericTypeMap.put(pt, pt);
        genericTypeMap.put(pt.getRawType(), pt);
      } else {
        genericTypeMap.put(type, type);
      }
      return true;
    }
  }
}
