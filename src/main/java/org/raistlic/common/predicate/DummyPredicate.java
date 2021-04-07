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

import java.util.function.Predicate;

/**
 * Dummy singleton implementations for {@link Predicate} that blindly returns a pre-defined check value.
 */
@SuppressWarnings("rawtypes")
enum DummyPredicate implements Predicate {

  /**
   * The immutable (singleton) instance of {@link Predicate} that simply returns {@code true} for
   * all tests.
   */
  TRUE(true),

  /**
   * The immutable (singleton) instance of {@link Predicate} that simply returns {@code false} for
   * all tests.
   */
  FALSE(false);

  private final boolean dummyResult;

  DummyPredicate(boolean dummyResult) {

    this.dummyResult = dummyResult;
  }

  @Override
  public boolean test(Object o) {

    return dummyResult;
  }
}
