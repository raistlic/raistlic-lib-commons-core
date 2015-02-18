package org.raistlic.common.precondition;

/**
 * @author Lei.C (2015-02-17)
 */
public final class Expectation {

  public static interface OfObject {

    default void isNull() {
      isNull(null);
    }

    void isNull(String message);

    default void notNull() {
      notNull(null);
    }

    void notNull(String message);

    default void equalTo(Object target) {
      equalTo(target, null);
    }

    void equalTo(Object target, String message);

    void notEqualTo(Object target);
  }

  public static interface OfString extends OfObject {

    void isEmpty();

    void notEmpty();

    void hasLength(int length);

    void minLength(int minLength);

    void maxLength(int maxLength);

    void hasEncoding(String encodingName);
  }

  public static interface OfBoolean {

    void isTrue();

    void isFalse();
  }

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
