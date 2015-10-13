package org.raistlic.common.util;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import org.raistlic.common.precondition.InvalidParameterException;

import java.util.function.Predicate;

import static org.fest.assertions.Assertions.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

/**
 * @author Lei Chen (2015-10-13)
 */
@RunWith(JUnit4.class)
@SuppressWarnings("unchecked")
public class PredicateNotWrapperTest {

  @Test(expected = InvalidParameterException.class)
  public void constructorWithNullOriginal() {

    new PredicateNotWrapper<Object>(null);
  }

  @Test
  public void testWhenOriginalTestTrue() {

    Object target = new Object();
    Predicate<Object> original = mock(Predicate.class);
    doReturn(true).when(original).test(any());

    boolean actual = new PredicateNotWrapper<Object>(original).test(target);

    assertThat(actual).isFalse();
    verify(original).test(target);
  }

  @Test
  public void testWhenOriginalTestFalse() {

    Object target = new Object();
    Predicate<Object> original = mock(Predicate.class);
    doReturn(false).when(original).test(any());

    boolean actual = new PredicateNotWrapper<Object>(original).test(target);

    assertThat(actual).isTrue();
    verify(original).test(target);
  }
}
