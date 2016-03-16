package org.raistlic.common.reflection;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

/**
 * The method holds static utility methods for dealing with {@link Type} .
 *
 * @author Lei Chen (2016-03-17)
 */
public final class Types {

  /**
   * The method tries to find and return a {@link ParameterizedType} in the inheritance hierarchy
   * (ancestors) of the specified {@code concreteType}, who's raw type is the specified {@code rawType} .
   *
   * @param concreteType the concrete implementation type to start with the search, cannot be
   *        {@code null}, and must be a subtype of the {@code rawType} .
   * @param rawType the raw type of the {@link ParameterizedType}  to search, cannot be {@code null}
   *                and must be a super type of the {@code concreteType} .
   * @return the {@link ParameterizedType} found, or {@code null} if none found.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when either parameter is
   *         {@code null}, or when {@code concreteType} is not a sub type of {@code rawType} .
   */
  public static ParameterizedType findParameterizedTypeFor(Class<?> concreteType, Class<?> rawType) {

    throw new UnsupportedOperationException();
  }

  private Types() { }
}
