package org.raistlic.common.precondition;

/**
 * @author Lei.C (2015-02-17)
 */
public interface ObjectExpectation {

  void isNull();

  void isNotNull();

  void isEqualTo(Object target);

  void isNotEqualTo(Object target);
}
