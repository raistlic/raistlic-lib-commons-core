package org.raistlic.common.reflection;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import org.raistlic.common.precondition.InvalidParameterException;
import org.raistlic.common.reflection.fixtures.AnotherClass;
import org.raistlic.common.reflection.fixtures.FooBar;
import org.raistlic.common.reflection.fixtures.GenericComparableFoo;
import org.raistlic.common.reflection.fixtures.GenericFoo;

import java.lang.reflect.ParameterizedType;

/**
 * Unit test for {@link Types} .
 *
 * @author Lei Chen (2016-03-17)
 */
@RunWith(JUnit4.class)
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
   * The method should be able to find the correct {@link ParameterizedType} instance for proper
   * parameters.
   */
  @Test
  public void findParameterizedTypeForExpectedWhenFound() {

    ParameterizedType found = Types.findParameterizedTypeFor(FooBar.class, GenericComparableFoo.class);
    // TODO park here until our own assertion utilities for test is done.
  }
}
