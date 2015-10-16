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

import org.raistlic.common.Factory;
import org.raistlic.common.precondition.Precondition;

import java.util.function.Predicate;

/**
 * @author Lei Chen (2015-10-13)
 * @since 1.3
 */
public final class PredicateBuilder<E> implements Factory<Predicate<E>> {

  private Predicate<E> predicate;

  public PredicateBuilder(Predicate<E> base) {

    Precondition.param(base, "base").notNull();

    this.predicate = base;
  }

  public PredicateBuilder<E> not() {

    this.predicate = Predicates.not(predicate);
    return this;
  }

  public PredicateBuilder<E> and(Predicate<? super E> predicate) {

    this.predicate = Predicates.and(this.predicate, predicate);
    return this;
  }

  public PredicateBuilder<E> or(Predicate<? super E> predicate) {

    this.predicate = Predicates.or(this.predicate, predicate);
    return this;
  }

  @Override
  public Predicate<E> build() {

    return this.predicate;
  }

  @Override
  public boolean isReady() {

    return true;
  }
}
