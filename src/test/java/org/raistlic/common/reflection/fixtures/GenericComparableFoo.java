package org.raistlic.common.reflection.fixtures;

public interface GenericComparableFoo<C extends Comparable<C>> extends GenericFoo<C> {

  @Mark("setFooSecond in GenericComparableFoo")
  default void setFooSecond(C foo) {

  }
}
