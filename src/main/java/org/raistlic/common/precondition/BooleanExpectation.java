package org.raistlic.common.precondition;

/**
 * @author Lei.C (2015-02-17)
 */
public interface BooleanExpectation {

  public void isTrue();

  public void isTrue(String message);

  public void isFalse();

  public void isFalse(String message);
}
