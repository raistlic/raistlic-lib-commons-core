package org.raistlic.common.reflection.fixtures;

public class ComparableFooLong implements GenericComparableFoo<Long> {

  @Override
  public Long getFoo() {

    throw new UnsupportedOperationException();
  }

  @Override
  public void setFooFirst(Long foo) {

    throw new UnsupportedOperationException();
  }

  @Override
  public String fooString(Long foo) {

    throw new UnsupportedOperationException();
  }
}
