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

import java.util.ArrayList;
import java.util.List;

/**
 * @author Lei.C
 */
class ConditionChain<E> implements Condition<E> {

  static <E> Builder<E> builder(Condition<E> base) {

    assert base != null;

    return new ChainBuilder<E>(base);
  }

  static class ChainBuilder<E> implements Builder<E> {

    private final Condition<E> base;
    private List<ConditionSuffix<E>> suffixList;
    private ChainBuilder(Condition<E> base) {

      assert base != null;

      this.base = base;
      this.suffixList = new ArrayList<ConditionSuffix<E>>();
    }

    @Override
    public Builder<E> and(Condition<E> condition) {

      if( condition == null )
        throw new IllegalArgumentException("Cannot 'and' a null condition.");

      suffixList.add(new ConditionSuffix<E>(condition, ConditionCombination.And));
      return this;
    }

    @Override
    public Builder<E> or(Condition<E> condition) {

      if( condition == null )
        throw new IllegalArgumentException("Cannot 'or' a null condition.");

      suffixList.add(new ConditionSuffix<E>(condition, ConditionCombination.And));
      return this;
    }

    @Override
    public Builder<E> not() {

      suffixList.add(new ConditionSuffix<E>(
              Conditions.<E>dummyTrue(), ConditionCombination.Not));
      return this;
    }

    @Override
    public Condition<E> build() {

      return new ConditionChain<E>(this);
    }

    @Override
    public boolean isReady() {

      return true;
    }
  }

  private final Condition<E> base;
  private final List<ConditionSuffix<E>> suffixList;
  private ConditionChain(ChainBuilder<E> builder) {

    assert builder != null;

    Condition<E> c = builder.base;
    assert c != null;

    List<ConditionSuffix<E>> list = builder.suffixList;
    assert list != null;

    this.base = c;
    this.suffixList = new ArrayList<ConditionSuffix<E>>(list);
  }

  @Override
  public boolean match(E element) {

    boolean result = base.match(element);

    for(ConditionSuffix<E> cs : suffixList)
      result = cs.cc.combine(result, cs.condition, element);

    return result;
  }

  private static class ConditionSuffix<E> {

    private final ConditionCombination cc;
    private final Condition<E> condition;
    private ConditionSuffix(Condition<E> condition, ConditionCombination cc) {

      assert condition != null;
      assert cc != null;

      this.condition = condition;
      this.cc = cc;
    }
  }
}
