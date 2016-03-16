package org.raistlic.common.reflection.fixtures;

/**
 * @author Lei Chen (2016-03-12)
 */
public class TestClass implements TestInterface<Integer> {

  @Override
  public void testVoidMethod() {

    System.out.println("testVoidMethod");
  }

  @Override
  public Integer testGenericReturnMethod() {

    throw new UnsupportedOperationException();
  }

  @Override
  public void testMethodWithGenericParam(Integer param, String param2) {

    System.out.println("testMethodWithGenericParam");
  }

  @Override
  public void testMethodWithObjectParam(Object param) {

    System.out.println("testMethodWithObjectParam: Object");
  }

  public void testMethodWithObjectParam(String param) {

    System.out.println("testMethodWithObjectParam: String");
  }
}
