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
