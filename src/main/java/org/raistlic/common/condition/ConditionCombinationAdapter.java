/*
 * Copyright 2013 Lei CHEN (raistlic@gmail.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.raistlic.common.condition;

/**
 * @author Lei.C
 */
class ConditionCombinationAdapter<E> implements Condition<E> {

  private final Condition<E> left;

  private final Condition<E> right;

  private final ConditionCombination cc;

  ConditionCombinationAdapter(Condition<E> left, Condition<E> right, ConditionCombination cc) {

    assert left != null;
    assert right != null;
    assert cc != null;

    this.left = left;
    this.right = right;
    this.cc = cc;
  }

  @Override
  public boolean match(E element) {

    return cc.combine(left.match(element), right, element);
  }
}
