package org.raistlic.common.reflection.fixtures;

public class FooBar extends AbstractFooBar<String, Integer> {

  @Override
  public Integer getBar() {

    return 1;
  }

  @Override
  public void setBar(Integer bar) {

  }

  @Override
  public String getFoo() {

    return "foo";
  }

  @Override
  public void setFooFirst(String foo) {

  }
}
