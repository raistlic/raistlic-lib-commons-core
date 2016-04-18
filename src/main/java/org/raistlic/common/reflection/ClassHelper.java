package org.raistlic.common.reflection;

import org.raistlic.common.precondition.Precondition;

import java.lang.annotation.Annotation;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * A helper class that binds to a specified {@link Class} instance, and help with getting different types of information
 * from it.
 *
 * @param <E> the actual class type.
 */
public final class ClassHelper<E> {

  /**
   * The factory method that creates a {@link ClassHelper} instance with the specified {@code targetClass} .
   *
   * @param targetClass the class to be wrapped in the helper instance, cannot be {@code null}.
   * @param <E> the actual type of the {@code targetClass} .
   * @return the helper instance created.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code targetClass} is {@code null}.
   */
  public static <E> ClassHelper<E> of(Class<E> targetClass) {

    Precondition.param(targetClass, "targetClass").isNotNull();
    return new ClassHelper<>(targetClass);
  }

  private final Class<E> targetClass;

  private ClassHelper(Class<E> targetClass) {

    assert targetClass != null;
    this.targetClass = targetClass;
  }

  /**
   * The method returns the wrapped {@link Class} instance.
   *
   * @return the wrapped {@link Class} instance.
   */
  public Class<E> getTargetClass() {

    return targetClass;
  }

  /**
   * The method returns all of the {@code targetClass}' methods as a {@link List} .
   *
   * @return all {@code targetClass}' methods as a {@link List}, empty list if none methods found, the method never
   *         returns {@code null}.
   */
  public List<Method> getAllMethods() {

    Set<Method> set = new HashSet<>();
    set.addAll(Arrays.asList(targetClass.getMethods()));
    set.addAll(Arrays.asList(targetClass.getDeclaredMethods()));
    return new ArrayList<>(set);
  }

  /**
   * The method returns all of {@code targetClass}' methods as a {@link MethodStream} instance.
   * See also {@link MethodStream} .
   *
   * @return all {@code targetClass}' methods as a {@link MethodStream} instance, never {@code null}.
   */
  public MethodStream getAllMethodsAsStream() {

    return MethodStream.of(getAllMethods());
  }

  /**
   * The method returns the {@code targetClass}' declared methods as a {@link List} .
   *
   * @return the {@code targetClass}' declared methods as a {@link List} , or empty list when none found, the method
   *         never returns {@code null}.
   */
  public List<Method> getDeclaredMethods() {

    return Collections.unmodifiableList(Arrays.asList(targetClass.getDeclaredMethods()));
  }

  /**
   * The method returns the {@code targetClass}' declared methods as a {@link MethodStream} instance.
   * See also {@link MethodStream} .
   *
   * @return the {@code targetClass}' declared methods as a {@link MethodStream} instance, never {@code null}.
   */
  public MethodStream getDeclaredMethodsAsStream() {

    return MethodStream.of(getDeclaredMethods());
  }

  /**
   * The method returns {@code targetClass} ' constructors as a {@link List} .
   * See also {@link ConstructorStream} .
   *
   * @return {@code targetClass} ' constructors as a {@link List}, never {@code null} or empty.
   */
  @SuppressWarnings("unchecked")
  public List<Constructor<E>> getConstructors() {

    Constructor[] constructors = targetClass.getConstructors();
    return Arrays.asList(constructors);
  }

  /**
   * The method returns {@code targetClass} ' constructors as a {@link ConstructorStream} instance.
   * See also {@link ConstructorStream} .
   *
   * @return {@code targetClass} ' constructors as a {@link ConstructorStream} instance, never {@code null}.
   */
  public ConstructorStream<E> getConstructorsAsStream() {

    return ConstructorStream.of(getConstructors());
  }

  /**
   * The method returns {@code targetClass} ' all fields as a {@link List} .
   *
   * @return {@code targetClass} ' all fields as a {@link List}
   */
  public List<Field> getAllFields() {

    Set<Field> fields = new HashSet<>();
    fields.addAll(Arrays.asList(targetClass.getFields()));
    fields.addAll(Arrays.asList(targetClass.getDeclaredFields()));
    return new ArrayList<>(fields);
  }

  /**
   * The method returns {@code targetClass}' all fields as a {@link FieldStream} instance.
   * See also {@link FieldStream} .
   *
   * @return {@code targetClass} ' all fields as a {@link FieldStream} instance, never {@code null}.
   */
  public FieldStream getAllFieldsAsStream() {

    return FieldStream.of(getAllFields());
  }

  /**
   * The method returns {@code targetClass} ' declared fields as a {@link List} .
   *
   * @return {@code targetClass} ' declared methods as a {@link List} .
   */
  public List<Field> getDeclaredFields() {

    return Arrays.asList(targetClass.getDeclaredFields());
  }

  /**
   * The method returns {@code targetClass} ' declared fields as a {@link FieldStream} instance.
   * See also {@link FieldStream} .
   *
   * @return {@code targetClass} ' declared fields as a {@link FieldStream} instance, never {@code null}.
   */
  public FieldStream getDeclaredFieldsAsStream() {

    return FieldStream.of(getDeclaredFields());
  }
}
