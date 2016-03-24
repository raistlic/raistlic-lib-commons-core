package org.raistlic.common.reflection;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import org.raistlic.common.precondition.InvalidParameterException;
import org.raistlic.common.reflection.fixtures.FooBar;

import static org.raistlic.common.postcondition.Postcondition.assertThat;

/**
 * Unit tests for {@link ClassHelper} .
 */
@RunWith(JUnit4.class)
public class ClassHelperTest {

  /**
   * The static factory method should throw {@link InvalidParameterException} when the parameter {@code targetClass}
   * is {@code null}.
   */
  @Test(expected = InvalidParameterException.class)
  public void factoryMethodWhenTargetClassIsNull() {

    ClassHelper.of(null);
  }

  @Test
  public void factoryMethodExpected() {

    ClassHelper<FooBar> classHelper = ClassHelper.of(FooBar.class);
    assertThat(classHelper).isNotNull();
    assertThat(classHelper.getTargetClass()).isEqualTo(FooBar.class);
  }
}
