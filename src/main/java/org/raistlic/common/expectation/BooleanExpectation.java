package org.raistlic.common.expectation;

public interface BooleanExpectation extends Expectation<Boolean, BooleanExpectation> {

  void isTrue();

  void isTrue(String message);

  void isFalse();

  void isFalse(String message);
}
