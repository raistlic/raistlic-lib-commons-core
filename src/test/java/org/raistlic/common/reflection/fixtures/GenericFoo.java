package org.raistlic.common.reflection.fixtures;

@Mark("GenericFoo<E>")
public interface GenericFoo<E> {

  E getFoo();

  void setFooFirst(E foo);

  void setFooSecond(E foo);

  @Mark("fooString in GenericFoo")
  String fooString(E foo);
}
