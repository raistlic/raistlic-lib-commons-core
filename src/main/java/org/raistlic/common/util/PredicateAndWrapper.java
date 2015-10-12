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

import org.raistlic.common.precondition.Precondition;

import java.util.function.Predicate;

/**
 * @author Lei Chen (2015-10-13)
 * @since 1.3
 */
public final class PredicateAndWrapper<E> implements Predicate<E> {

  private final Predicate<? super E> left;

  private final Predicate<? super E> right;

  public PredicateAndWrapper(Predicate<? super E> left, Predicate<? super E> right) {

    Precondition.param(left, "left").notNull();
    Precondition.param(right, "right").notNull();

    this.left = left;
    this.right = right;
  }

  @Override
  public boolean test(E e) {

    return left.test(e) && right.test(e);
  }
}
