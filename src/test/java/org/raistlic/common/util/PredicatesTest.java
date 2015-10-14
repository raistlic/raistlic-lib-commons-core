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

/**
 * @author Lei Chen (2015-10-14)
 */
@RunWith(JUnit4.class)
public class PredicatesTest {

  @Test
  public void dummyTrueExpected() {

    Predicate<String> predicate = Predicates.dummyTrue();

    assertThat(predicate).isNotNull();
    assertThat(predicate == Predicates.<String>dummyTrue()).isTrue();
    assertThat(predicate == Predicates.<String>dummyTrue()).isTrue();
    assertThat(predicate == Predicates.<String>dummyTrue()).isTrue();
  }

  @Test
  public void dummyFalseExpected() {

    Predicate<String> predicate = Predicates.dummyFalse();

    assertThat(predicate).isNotNull();
    assertThat(predicate == Predicates.<String>dummyFalse()).isTrue();
    assertThat(predicate == Predicates.<String>dummyFalse()).isTrue();
    assertThat(predicate == Predicates.<String>dummyFalse()).isTrue();
  }

  @Test(expected = InvalidParameterException.class)
  public void notWrapperWithNullPredicate() {

    Predicates.not(null);
  }

  @Test
  public void notWrapperExpected() {

    Predicate<Object> predicate = Predicates.not(Predicates.dummyTrue());
    assertThat(predicate).isNotNull();
  }

  @Test(expected = InvalidParameterException.class)
  public void andWrapperWithNullLeft() {

    Predicates.and(null, Predicates.dummyTrue());
  }

  @Test(expected = InvalidParameterException.class)
  public void andWrapperWithNullRight() {

    Predicates.and(Predicates.dummyTrue(), null);
  }

  @Test
  public void andWrapperExpected() {

    Predicate<Object> predicate = Predicates.and(Predicates.dummyTrue(), Predicates.dummyTrue());
    assertThat(predicate).isNotNull();
  }

  @Test(expected = InvalidParameterException.class)
  public void orWrapperWithNullLeft() {

    Predicates.or(null, Predicates.dummyTrue());
  }

  @Test(expected = InvalidParameterException.class)
  public void orWrapperWithNullRight() {

    Predicates.or(Predicates.dummyTrue(), null);
  }

  @Test
  public void orWrapperExpected() {

    Predicate<Object> predicate = Predicates.or(Predicates.dummyTrue(), Predicates.dummyTrue());
    assertThat(predicate).isNotNull();
  }

  @Test(expected = InvalidParameterException.class)
  public void builderWithNullBase() {

    Predicates.builder(null);
  }

  @Test
  public void builderExpected() {

    PredicateBuilder<Object> builder = Predicates.builder(Predicates.dummyTrue());
    assertThat(builder).isNotNull();
  }
}
