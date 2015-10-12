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
 *
 * @author Lei CHEN
 * @since 1.3
 */
public final class Predicates {
  
  @SuppressWarnings({"unchecked", "rawtypes"})
  public static <E> Predicate<E> dummyTrue() {
    
    return (Predicate<E>) DummyPredicate.TRUE;
  }
  
  @SuppressWarnings({"unchecked", "rawtypes"})
  public static <E> Predicate<E> dummyFalse() {
    
    return (Predicate<E>) DummyPredicate.FALSE;
  }
  
  public static <E> Predicate<E> not(Predicate<? super E> predicate) {

    Precondition.param(predicate, "predicate").notNull();
    
    return new PredicateNotWrapper<E>(predicate);
  }
  
  public static <E> Predicate<E> and(Predicate<? super E> left, Predicate<? super E> right) {

    Precondition.param(left, "left").notNull();
    Precondition.param(right, "right").notNull();

    return new PredicateAndWrapper<E>(left, right);
  }
  
  public static <E> Predicate<E> or(Predicate<? super E> left, Predicate<? super E> right) {

    Precondition.param(left, "left").notNull();
    Precondition.param(right, "right").notNull();

    return new PridicateOrWrapper<E>(left, right);
  }
  
  public static <E> PredicateBuilder<E> builder(Predicate<? super E> base) {
    
    Precondition.param(base, "base").notNull();

    return new PredicateBuilder<E>(Predicates.<E>dummyTrue()).and(base);
  }
  
  private Predicates() { }
}


