package org.raistlic.common.reflection.fixtures;

import java.io.Serializable;

public abstract class AbstractFooBar<S extends Serializable & Comparable<S>, N extends Number>
        implements GenericComparableFoo<S>, GenericBar<N> {

  @Mark("barString in AbstractFooBar")
  @Override
  public String barString(N bar) {

    return "bar string from AbstractFooBar";
  }

  @Override
  public String fooString(S foo) {

    return "foo string from AbstractFooBar";
  }
}
