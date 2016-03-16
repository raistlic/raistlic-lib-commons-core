package org.raistlic.common.reflection.fixtures;

public interface GenericBar<N extends Number> {

  N getBar();

  @Mark("setBar in GenericBar")
  void setBar(N bar);

  @Mark("barString in GenericBar")
  String barString(N bar);
}
