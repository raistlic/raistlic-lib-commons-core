package org.raistlic.common.precondition;

import java.util.Objects;

/**
 * @author Lei.C (2015-02-17)
 */
class ParamObjectExpectation implements ObjectExpectation {

  private final Object parameter;

  private final String name;

  ParamObjectExpectation(Object parameter, String name) {

    this.parameter = parameter;
    this.name = name;
  }

  @Override
  public void isNull() {

    if (parameter != null) {
      String message = name == null ? "" : "'" + name + "' should be null but it's not.";
      throw new InvalidParameterException(message);
    }
  }

  @Override
  public void isNotNull() {

    if (parameter == null) {
      String message = name == null ? "" : "'" + name + "' should not be null.";
      throw new InvalidParameterException(message);
    }
  }

  @Override
  public void isEqualTo(Object target) {

    if (!Objects.equals(parameter, target)) {
      String message = name == null ? "" : "'" + name + "' should be equal to " + target + ", but was " + parameter;
      throw new InvalidParameterException(message);
    }
  }

  @Override
  public void isNotEqualTo(Object target) {

    if (Objects.equals(parameter, target)) {
      String message = name == null ? "" : "'" + name + "' should not be " + target + ".";
      throw new InvalidParameterException(message);
    }
  }
}
