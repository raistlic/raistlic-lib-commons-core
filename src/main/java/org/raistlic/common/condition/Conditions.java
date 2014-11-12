/*
 * Copyright 2013 Lei CHEN (raistlic@gmail.com)
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
package org.raistlic.common.condition;

/**
 *
 * @author Lei.C
 */
public class Conditions {
  
  @SuppressWarnings({"unchecked", "rawtypes"})
  public static <E> Condition<E> dummyTrue() {
    
    return (Condition<E>)DummyCondition.True;
  }
  
  @SuppressWarnings({"unchecked", "rawtypes"})
  public static <E> Condition<E> dummyFalse() {
    
    return (Condition<E>)DummyCondition.False;
  }
  
  public static <E> Condition<E> not(Condition<E> condition) {
    
    if( condition == null )
      throw new IllegalArgumentException(
              "Cannot adapt not condition for a null condition.");
    
    return new NotConditionAdapter<E>(condition);
  }
  
  public static <E> Condition<E> and(Condition<E> left, Condition<E> right) {
    
    if( left == null )
      throw new IllegalArgumentException("left condition cannot be null.");
    
    if( right == null )
      throw new IllegalArgumentException("right condition cannot be null.");
    
    return new ConditionCombinationAdapter<E>(left, right, ConditionCombination.And);
  }
  
  public static <E> Condition<E> or(Condition<E> left, Condition<E> right) {
    
    if( left == null )
      throw new IllegalArgumentException("left condition cannot be null.");
    
    if( right == null )
      throw new IllegalArgumentException("right condition cannot be null.");
    
    return new ConditionCombinationAdapter<E>(left, right, ConditionCombination.Or);
  }
  
  public static <E> Condition.Builder<E> builder(Condition<E> base) {
    
    if( base == null )
      throw new IllegalArgumentException("Cannot build a condition chain on a null base.");
    
    return ConditionChain.builder(base);
  }
  
  private Conditions() {}
}


