package org.raistlic.common.util;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import static org.fest.assertions.Assertions.assertThat;

/**
 * @author Lei Chen (2015-10-13)
 */
@RunWith(JUnit4.class)
public class DummyPredicateTest {

  @Test
  public void testDummyTrue() {

    boolean actual = DummyPredicate.TRUE.test(new Object());
    assertThat(actual).isTrue();
  }

  @Test
  public void testDummyTrueWithNullTarget() {

    boolean actual = DummyPredicate.TRUE.test(null);
    assertThat(actual).isTrue();
  }

  @Test
  public void testDummyFalse() {

    boolean actual = DummyPredicate.FALSE.test(new Object());
    assertThat(actual).isFalse();
  }

  @Test
  public void testDummyFalseWithNullTarget() {

    boolean actual = DummyPredicate.FALSE.test(null);
    assertThat(actual).isFalse();
  }
}
