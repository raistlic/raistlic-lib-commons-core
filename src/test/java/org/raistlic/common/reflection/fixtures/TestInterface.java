package org.raistlic.common.reflection.fixtures;

/**
 * @author Lei Chen (2016-03-12)
 */
public interface TestInterface<E extends Number> {

  void testVoidMethod();

  E testGenericReturnMethod();

  void testMethodWithGenericParam(E param, String param2);

  void testMethodWithGenericParam(Integer param, String param2);

  void testMethodWithObjectParam(Object param);
}
