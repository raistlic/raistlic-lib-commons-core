package org.raistlic.common.expectation;

public interface BooleanExpectation {

  void isTrue();

  void isTrue(String message);

  void isFalse();

  void isFalse(String message);

  void isEqualTo(boolean expected);

  void isEqualTo(boolean expected, String message);
}
