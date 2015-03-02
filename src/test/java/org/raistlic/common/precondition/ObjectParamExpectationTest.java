package org.raistlic.common.precondition;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * @author Lei.C (2015-03-02)
 */
public class ObjectParamExpectationTest {

  @Rule
  public ExpectedException thrown = ExpectedException.none();

  @Test
  public void testIsNullWithNullParam() {

    Precondition.param(null).isNull();
    Precondition.param(null, "name").isNull();
    Precondition.param(null).isNull("message");
    Precondition.param(null, "name").isNull("message");
  }

  @Test
  public void testIsNullWithNonNullParamNoNameNoMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param(new Object()).isNull();
  }

  @Test
  public void testIsNullWithNonNullParamWithNameNoMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param(new Object(), "name").isNull();
  }

  @Test
  public void testIsNullWithNonNullParamNoNameWithMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param(new Object()).isNull("message");
  }

  @Test
  public void testIsNullWithNonNullParamWithNameWithMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param(new Object(), "name").isNull("message");
  }

  @Test
  public void testNotNullWithNonNullParameter() {

    Precondition.param(new Object()).notNull();
    Precondition.param(new Object()).notNull("message");
    Precondition.param(new Object(), "name").notNull();
    Precondition.param(new Object(), "name").notNull("message");
  }

  @Test
  public void testNotNullWithNullParamNoNameNoMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param(null).notNull();
  }

  @Test
  public void testNotNullWithNullParamWithNameNoMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param(null, "name").notNull();
  }

  @Test
  public void testNotNullWithNullParamNoNameWithMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param(null).notNull("message");
  }

  @Test
  public void testNotNullWithNullParamWithNameWithMessage() {

    thrown.expect(InvalidParameterException.class);
    Precondition.param(null, "name").notNull("message");
  }

  @Test
  public void testEqualToWithEqualParams() {

    Object param1 = new Object();

    Precondition.param(param1).equalTo(param1);
    Precondition.param(param1, "name").equalTo(param1);
    Precondition.param(param1).equalTo(param1, "message");
    Precondition.param(param1, "name").equalTo(param1, "message");
  }

  @Test
  public void testEqualToWithNotEqualParamsNoNameNoMessage() {

    Object param1 = new Object();
    Object param2 = new Object();

    thrown.expect(InvalidParameterException.class);
    Precondition.param(param1).equalTo(param2);
  }

  @Test
  public void testEqualToWithNotEqualParamsWithNameNoMessage() {

    Object param1 = new Object();
    Object param2 = new Object();

    thrown.expect(InvalidParameterException.class);
    Precondition.param(param1, "name").equalTo(param2);
  }

  @Test
  public void testEqualToWithNotEqualParamsNoNameWithMessage() {

    Object param1 = new Object();
    Object param2 = new Object();

    thrown.expect(InvalidParameterException.class);
    Precondition.param(param1).equalTo(param2, "message");
  }

  @Test
  public void testEqualToWithNotEqualParamsWithNameWithMessage() {

    Object param1 = new Object();
    Object param2 = new Object();

    thrown.expect(InvalidParameterException.class);
    Precondition.param(param1, "name").equalTo(param2, "message");
  }

  @Test
  public void testNotEqualToWithNotEqualParams() {

    Object param1 = new Object();
    Object param2 = new Object();

    Precondition.param(param1).notEqualTo(param2);
    Precondition.param(param1, "name").notEqualTo(param2);
    Precondition.param(param1).notEqualTo(param2, "message");
    Precondition.param(param1, "name").notEqualTo(param2, "message");
  }

  @Test
  public void testNotEqualToWithEqualParamsNoNameNoMessage() {

    Object param1 = new Object();

    thrown.expect(InvalidParameterException.class);
    Precondition.param(param1).notEqualTo(param1);
  }

  @Test
  public void testNotEqualToWithEqualParamsWithNameNoMessage() {

    Object param1 = new Object();

    thrown.expect(InvalidParameterException.class);
    Precondition.param(param1, "name").notEqualTo(param1);
  }

  @Test
  public void testNotEqualToWithEqualParamsNoNameWithMessage() {

    Object param1 = new Object();

    thrown.expect(InvalidParameterException.class);
    Precondition.param(param1).notEqualTo(param1, "message");
  }

  @Test
  public void testNotEqualToWithEqualParamsWithNameWithMessage() {

    Object param1 = new Object();

    thrown.expect(InvalidParameterException.class);
    Precondition.param(param1, "name").notEqualTo(param1, "message");
  }
}