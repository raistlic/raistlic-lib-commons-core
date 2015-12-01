/*
 * Copyright 2015 Lei CHEN (raistlic@gmail.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.raistlic.common.predicate;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import org.raistlic.common.precondition.InvalidParameterException;

import java.util.function.Predicate;

import static org.fest.assertions.api.Assertions.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * @author Lei Chen (2015-10-13)
 */
@RunWith(JUnit4.class)
@SuppressWarnings("unchecked")
public class PredicateAndWrapperTest {

  @Test(expected = InvalidParameterException.class)
  public void constructorWithNullLeft() {

    new PredicateAndWrapper<Object>(null, Predicates.dummyTrue());
  }

  @Test(expected = InvalidParameterException.class)
  public void constructorWithNullRight() {

    new PredicateAndWrapper<Object>(Predicates.dummyTrue(), null);
  }

  @Test
  public void testWhenBothTestsFalse() {

    Object target = new Object();
    Predicate<Object> left = mock(Predicate.class);
    Predicate<Object> right = mock(Predicate.class);
    doReturn(false).when(left).test(target);
    doReturn(false).when(right).test(target);

    boolean actual = new PredicateAndWrapper<Object>(left, right).test(target);

    assertThat(actual).isFalse();
    verify(left).test(target);
    verify(right, never()).test(any());
  }

  @Test
  public void testWhenLeftTestFalse() {

    Object target = new Object();
    Predicate<Object> left = mock(Predicate.class);
    Predicate<Object> right = mock(Predicate.class);
    doReturn(false).when(left).test(target);
    doReturn(true).when(right).test(target);

    boolean actual = new PredicateAndWrapper<Object>(left, right).test(target);

    assertThat(actual).isFalse();
    verify(left).test(target);
    verify(right, never()).test(any());
  }

  @Test
  public void testWhenRightTestFalse() {

    Object target = new Object();
    Predicate<Object> left = mock(Predicate.class);
    Predicate<Object> right = mock(Predicate.class);
    doReturn(true).when(left).test(target);
    doReturn(false).when(right).test(target);

    boolean actual = new PredicateAndWrapper<Object>(left, right).test(target);

    assertThat(actual).isFalse();
    verify(left).test(target);
    verify(right).test(target);
  }

  @Test
  public void testWhenBothTestsTrue() {

    Object target = new Object();
    Predicate<Object> left = mock(Predicate.class);
    Predicate<Object> right = mock(Predicate.class);
    doReturn(true).when(left).test(target);
    doReturn(true).when(right).test(target);

    boolean actual = new PredicateAndWrapper<Object>(left, right).test(target);

    assertThat(actual).isTrue();
    verify(left).test(target);
    verify(right).test(target);
  }
}
