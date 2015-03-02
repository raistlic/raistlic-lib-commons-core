package org.raistlic.common.precondition;

import java.util.Objects;

/**
 * @author Lei.C (2015-02-18)
 */
public final class ParamExpectation {

  public final static class OfObject {

    private final Object param;

    private final String name;

    OfObject(Object param, String name) {

      this.param = param;
      this.name = name;
    }

    public void isNull() {

      isNull(null);
    }

    public void isNull(String message) {

      if (param != null) {

        if (message == null) {
          message = (name == null) ? "" : "'" + name + "' should be null, but was " + param;
        }
        error(message);
      }
    }

    public void notNull() {

      notNull(null);
    }

    public void notNull(String message) {

      if (param == null) {

        if (message == null) {
          message = (name == null) ? "" : "'" + name + "' should not be null.";
        }
        error(message);
      }
    }

    public void equalTo(Object target) {

      equalTo(target, null);
    }

    public void equalTo(Object target, String message) {

      if (!Objects.equals(this.param, target)) {

        if (message == null) {
          message = (name == null) ? "" : "'" + name + "' should be equal to '" + target + "', but is not.";
        }
        error(message);
      }
    }

    public void notEqualTo(Object target) {

      notEqualTo(target, null);
    }

    public void notEqualTo(Object target, String message) {

      if (Objects.equals(this.param, target)) {

        if (message == null) {
          message = (name == null) ? "" : "'" + name + "' should not be equal to '" + target + "'.";
        }
        error(message);
      }
    }
  }

  public final static class OfString {

    private final String param;

    private final String name;

    OfString(String param, String name) {

      this.param = param;
      this.name = name;
    }

    public void isNull() {

      isNull(null);
    }

    public void isNull(String message) {

      if (param != null) {

        if (message == null) {
          message = (name == null) ? "" : "'" + name + "' should be null, but was " + param;
        }
        error(message);
      }
    }

    public void notNull() {

      notNull(null);
    }

    public void notNull(String message) {

      if (param == null) {

        if (message == null) {
          message = (name == null) ? "" : "'" + name + "' should not be null.";
        }
        error(message);
      }
    }

    public void equalTo(String target) {

      equalTo(target, null);
    }

    public void equalTo(String target, String message) {

      if (!Objects.equals(this.param, target)) {

        if (message == null) {
          message = (name == null) ? "" : "'" + name + "' should be equal to '" + target + "', but is not.";
        }
        error(message);
      }
    }

    public void notEqualTo(Object target) {

      notEqualTo(target, null);
    }

    public void notEqualTo(Object target, String message) {

      if (Objects.equals(this.param, target)) {

        if (message == null) {
          message = (name == null) ? "" : "'" + name + "' should not be equal to '" + target + "'.";
        }
        error(message);
      }
    }

    public void isEmpty() {

      isEmpty(null);
    }

    public void isEmpty(String message) {

      if (param != null && !param.isEmpty()) {

        if (message == null) {
          message = (name == null) ? "" : "'" + name + "' should be empty but is: '" + param + "'";
        }
        error(message);
      }
    }
  }

  public final static class OfBoolean {

    private final boolean evaluative;

    private final String name;

    OfBoolean(boolean evaluative, String name) {

      this.evaluative = evaluative;
      this.name = name;
    }

    public void isTrue() {

      isTrue(null);
    }

    public void isTrue(String message) {

      if (!evaluative) {

        if (message == null) {

          message = this.name == null ? "" : "'" + name + "' should be 'true' but is 'false'.";
        }
        error(message);
      }
    }

    public void isFalse() {

      isFalse(null);
    }

    public void isFalse(String message) {

      if (evaluative) {

        if (message == null) {

          message = this.name == null ? "" : "'" + name + "' should be 'false' but is 'true'.";
        }
        error(message);
      }
    }
  }

  public static class OfInt {

    private final int evaluative;

    private final String name;

    OfInt(int evaluative, String name) {

      this.evaluative = evaluative;
      this.name = name;
    }

    public void equalTo(int target) {

      equalTo(target, null);
    }

    public void equalTo(int target, String message) {

      if (evaluative == target) {
        return;
      }
      if (message == null) {
        message = (name == null) ? "" : "'" + name + "' should be " + target + " but is " + evaluative;
      }
      error(message);
    }

    public void notEqualTo(int target) {

      notEqualTo(target, null);
    }

    public void notEqualTo(int target, String message) {

      if (evaluative != target) {
        return;
      }
      if (message == null) {
        message = (name == null) ? "" : "'" + name + "' should not be " + target;
      }
      error(message);
    }

    public void lessThan(int target) {

      lessThan(target, null);
    }

    public void lessThan(int target, String message) {

      if (evaluative < target) {
        return;
      }
      if (message == null) {
        message = (name == null) ? "" : "'" + name + "' should be less than " + target + ", but is not (" + evaluative + ").";
      }
      error(message);
    }

    public void noLessThan(int target) {

      noLessThan(target, null);
    }

    public void noLessThan(int target, String message) {

      if (evaluative >= target) {
        return;
      }
      if (message == null) {
        message = (name == null) ? "" : "'" + name + "' should be no less than " + target + ", but is not (" + evaluative + ").";
      }
      error(message);
    }

    public void greaterTHan(int target) {

      greaterThan(target, null);
    }

    public void greaterThan(int target, String message) {

      if (evaluative > target) {
        return;
      }
      if (message == null) {
        message = (name == null) ? "" : "'" + name + "' should be greater than " + target + ", but is not (" + evaluative + ").";
      }
      error(message);
    }
  }

  private static void error(String message) {

    throw new InvalidParameterException(message);
  }
}
