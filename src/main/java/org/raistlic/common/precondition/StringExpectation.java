package org.raistlic.common.precondition;

/**
 * @author Lei.C (2015-02-17)
 */
public interface StringExpectation extends ObjectExpectation {

  void beEmpty();

  void notEmpty();
}
