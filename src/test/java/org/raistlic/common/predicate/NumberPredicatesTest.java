package org.raistlic.common.predicate;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import org.raistlic.common.precondition.InvalidParameterException;

import java.util.function.Predicate;

import static org.fest.assertions.Assertions.assertThat;

/**
 * @author Lei Chen (2015-10-16)
 */
@RunWith(JUnit4.class)
public class NumberPredicatesTest {

  @Test(expected = InvalidParameterException.class)
  public void greaterThanWithNullReference() {

    NumberPredicates.greaterThan(null);
  }

  @Test
  public void greaterThanExpected() {

    Predicate<Integer> predicate = NumberPredicates.greaterThan(123);

    assertThat(predicate).isNotNull();
    assertThat(predicate.test(Integer.MIN_VALUE)).isFalse();
    assertThat(predicate.test(-124)).isFalse();
    assertThat(predicate.test(-123)).isFalse();
    assertThat(predicate.test(-122)).isFalse();
    assertThat(predicate.test(0)).isFalse();
    assertThat(predicate.test(122)).isFalse();
    assertThat(predicate.test(123)).isFalse();
    assertThat(predicate.test(124)).isTrue();
    assertThat(predicate.test(Integer.MAX_VALUE)).isTrue();
  }

  @Test
  public void greaterThanWithNullCandidate() {

    Predicate<Integer> predicate = NumberPredicates.greaterThan(123);
    assertThat(predicate.test(null)).isFalse();
  }

  @Test(expected = InvalidParameterException.class)
  public void greaterThanOrEqualToWithNullReference() {

    NumberPredicates.greaterThanOrEqualTo(null);
  }

  @Test
  public void greaterThanOrEqualToExpected() {

    Predicate<Integer> predicate = NumberPredicates.greaterThanOrEqualTo(123);

    assertThat(predicate).isNotNull();
    assertThat(predicate.test(Integer.MIN_VALUE)).isFalse();
    assertThat(predicate.test(-124)).isFalse();
    assertThat(predicate.test(-123)).isFalse();
    assertThat(predicate.test(-122)).isFalse();
    assertThat(predicate.test(0)).isFalse();
    assertThat(predicate.test(122)).isFalse();
    assertThat(predicate.test(123)).isTrue();
    assertThat(predicate.test(124)).isTrue();
    assertThat(predicate.test(Integer.MAX_VALUE)).isTrue();
  }

  @Test
  public void greaterThanOrEqualToWithNullCandidate() {

    Predicate<Integer> predicate = NumberPredicates.greaterThanOrEqualTo(123);
    assertThat(predicate.test(null)).isFalse();
  }

  @Test(expected = InvalidParameterException.class)
  public void lessThanWithNullReference() {

    NumberPredicates.lessThan(null);
  }

  @Test
  public void lessThanExpected() {

    Predicate<Integer> predicate = NumberPredicates.lessThan(123);

    assertThat(predicate).isNotNull();
    assertThat(predicate.test(Integer.MIN_VALUE)).isTrue();
    assertThat(predicate.test(-124)).isTrue();
    assertThat(predicate.test(-123)).isTrue();
    assertThat(predicate.test(-122)).isTrue();
    assertThat(predicate.test(0)).isTrue();
    assertThat(predicate.test(122)).isTrue();
    assertThat(predicate.test(123)).isFalse();
    assertThat(predicate.test(124)).isFalse();
    assertThat(predicate.test(Integer.MAX_VALUE)).isFalse();
  }

  @Test
  public void lessThanWithNullCandidate() {

    Predicate<Integer> predicate = NumberPredicates.lessThan(123);
    assertThat(predicate.test(null)).isFalse();
  }

  @Test(expected = InvalidParameterException.class)
  public void lessThanOrEqualToWithNullReference() {

    NumberPredicates.lessThanOrEqualTo(null);
  }

  @Test
  public void lessThanOrEqualToExpected() {

    Predicate<Integer> predicate = NumberPredicates.lessThanOrEqualTo(123);

    assertThat(predicate).isNotNull();
    assertThat(predicate.test(Integer.MIN_VALUE)).isTrue();
    assertThat(predicate.test(-124)).isTrue();
    assertThat(predicate.test(-123)).isTrue();
    assertThat(predicate.test(-122)).isTrue();
    assertThat(predicate.test(0)).isTrue();
    assertThat(predicate.test(122)).isTrue();
    assertThat(predicate.test(123)).isTrue();
    assertThat(predicate.test(124)).isFalse();
    assertThat(predicate.test(Integer.MAX_VALUE)).isFalse();
  }

  @Test
  public void lessThanOrEqualToWithNullCandidate() {

    Predicate<Integer> predicate = NumberPredicates.lessThanOrEqualTo(123);
    assertThat(predicate.test(null)).isFalse();
  }
}
