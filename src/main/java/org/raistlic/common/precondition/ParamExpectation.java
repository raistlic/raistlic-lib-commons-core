package org.raistlic.common.precondition;

/**
 * @author Lei.C (2015-02-18)
 */
final class ParamExpectation {

  final static class OfObject implements Expectation.OfObject {

    private final Object param;

    private final String name;

    OfObject(Object param, String name) {

      this.param = param;
      this.name = name;
    }

    @Override
    public void isNull(String message) {

      if (param != null) {

        if (message == null) {
          message = (name == null) ? "" : "'" + name + "' should be null, but was " + param;
        }
        error(message);
      }
    }

    @Override
    public void notNull(String message) {

      throw new UnsupportedOperationException();
    }

    @Override
    public void equalTo(Object target, String message) {

      throw new UnsupportedOperationException();
    }

    @Override
    public void notEqualTo(Object target) {

      throw new UnsupportedOperationException();
    }
  }

  private static void error(String message) {

    throw new InvalidParameterException(message);
  }
}
