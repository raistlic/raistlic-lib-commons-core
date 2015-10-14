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
