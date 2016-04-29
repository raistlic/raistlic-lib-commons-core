package org.raistlic.common.reflection;

import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import org.raistlic.common.postcondition.Postcondition;
import org.raistlic.common.precondition.InvalidParameterException;
import org.raistlic.common.reflection.fixtures.*;

import java.io.Serializable;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;

import static org.raistlic.common.postcondition.Postcondition.assertThat;

/**
 * Unit test for {@link Types} .
 *
 * @author Lei Chen (2016-03-17)
 */
@RunWith(JUnit4.class)
@Ignore("the reflection utils is a mess atm.")
public class TypesTest {

  /**
   * The method should fail with {@link InvalidParameterException} when {@code concreteType} parameter
   * is {@code null}.
   */
  @Test(expected = InvalidParameterException.class)
  public void findParameterizedTypeForWhenConcreteTypeIsNull() {

    Types.findParameterizedTypeFor(null, GenericFoo.class);
  }

  /**
   * The method should fail with {@link InvalidParameterException} when {@code rawType} parameter
   * is {@code null}.
   */
  @Test(expected = InvalidParameterException.class)
  public void findParameterizedTypeForWhenRawTypeIsNull() {

    Types.findParameterizedTypeFor(FooBar.class, null);
  }

  /**
   * The method should fail with {@link InvalidParameterException} when {@code rawType} is not
   * super type of {@code rawType} .
   */
  @Test(expected = InvalidParameterException.class)
  public void findParameterizedTypeForWhenRawTypeIsNotSuperTypeOfConcreteType() {

    Types.findParameterizedTypeFor(FooBar.class, AnotherClass.class);
  }

  /**
   * The method should be able to find the correct {@link ParameterizedType} instance when it's raw type is directly
   * implemented by the {@code concreteType} .
   */
  @Test
  public void findParameterizedTypeForExpectedWhenRawTypeIsDirectInterface() {

    ParameterizedType found = Types.findParameterizedTypeFor(ComparableFooLong.class, GenericComparableFoo.class);

    assertThat(found).isNotNull();
    assertThat(found.getRawType()).isEqualTo(GenericComparableFoo.class);

    Type[] typeArguments = found.getActualTypeArguments();
    assertThat(typeArguments).isNotNull();
    assertThat(typeArguments.length).isEqualTo(1);

    Type typeArgument = typeArguments[0];
    assertThat(typeArgument).isEqualTo(Long.class);
  }

  /**
   * The method should be able to find the correct {@link ParameterizedType} instance when it's raw type is directly
   * implemented by the {@code concreteType}, and when the {@code concreteType} is a generic class itself.
   */
  @Test
  public void findParameterizedTypeForExpectedWhenRawTypeIsDirectInterfaceForGenericClass() {

    ParameterizedType found = Types.findParameterizedTypeFor(AbstractFooBar.class, GenericComparableFoo.class);

    assertThat(found).isNotNull();
    assertThat(found.getRawType()).isEqualTo(GenericComparableFoo.class);
    assertThat(found.getOwnerType()).isNull();

    Type[] typeArguments = found.getActualTypeArguments();
    assertThat(typeArguments).isNotNull();
    assertThat(typeArguments.length).isEqualTo(1);

    Type typeArgument = typeArguments[0];
    assertThat(typeArgument).isInstanceOf(TypeVariable.class);

    @SuppressWarnings("unchecked")
    TypeVariable<Class<?>> typeVariable = (TypeVariable<Class<?>>) typeArgument;
    Class<?> genericDeclaration = typeVariable.getGenericDeclaration();
    Postcondition.<Class<?>>assertThat(genericDeclaration).isEqualTo(AbstractFooBar.class);
    String name = typeVariable.getName();
    Type[] bounds = typeVariable.getBounds();
    assertThat(bounds).isNotNull();
    assertThat(bounds.length).isEqualTo(2);
    assertThat(bounds[0]).isEqualTo(Serializable.class);
    assertThat(bounds[1]).isInstanceOf(ParameterizedType.class);
    assertThat(((ParameterizedType) bounds[1]).getRawType()).isEqualTo(Comparable.class);
  }

  /**
   * The method should be able to find the correct {@link ParameterizedType} instance when it's raw type is directly
   * extended by the {@code concreteType} .
   */
  @Test
  public void findParameterizedTypeForExpectedWhenRawTypeIsDirectSuperClass() {

    ParameterizedType found = Types.findParameterizedTypeFor(FooBar.class, AbstractFooBar.class);

    assertThat(found).isNotNull();
    assertThat(found.getRawType()).isEqualTo(AbstractFooBar.class);

    Type[] typeArguments = found.getActualTypeArguments();
    assertThat(typeArguments).isNotNull();
    assertThat(typeArguments.length).isEqualTo(2);
    assertThat(typeArguments[0]).isEqualTo(String.class);
    assertThat(typeArguments[1]).isEqualTo(Integer.class);
  }

  /**
   * The method should be able to find the correct {@link ParameterizedType} instance when it's raw type is an interface
   * that's implemented by one of the ancestors of the {@code concreteType} .
   */
  @Test
  public void findParameterizedTypeForExpectedWhenRawTypeIsInterfaceOfAncestorClass() {

    ParameterizedType found = Types.findParameterizedTypeFor(FooBar.class, GenericComparableFoo.class);

    assertThat(found).isNotNull();
    assertThat(found.getRawType()).isEqualTo(GenericComparableFoo.class);

    Type[] typeArguments = found.getActualTypeArguments();
    assertThat(typeArguments).isNotNull();
    assertThat(typeArguments.length).isEqualTo(1);

    Type typeArgument = typeArguments[0];
    assertThat(typeArgument).isInstanceOf(TypeVariable.class);

    TypeVariable<?> typeVariable = (TypeVariable<?>) typeArgument;
    assertThat(typeVariable.getGenericDeclaration()).isInstanceOf(Class.class);
    assertThat(typeVariable.getGenericDeclaration() == AbstractFooBar.class).isTrue();
  }
}
