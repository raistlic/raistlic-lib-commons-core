package org.raistlic.common.precondition;

/**
 * @author Lei.C (2015-02-17)
 */
public final class Expectation {

  public static interface OfInt {

    default void isEqualTo(int target) {

      isEqualTo(target, null);
    }

    void isEqualTo(int target, String message);

    default void isNotEqualTo(int target) {

      isNotEqualTo(target, null);
    }

    void isNotEqualTo(int target, String message);

    void isLessThan(int target, String message);

    void isNotLessThan(int target, String message);

    void isGreaterThan(int target, String message);

    void isNotGreaterThan(int target, String message);
  }
}
