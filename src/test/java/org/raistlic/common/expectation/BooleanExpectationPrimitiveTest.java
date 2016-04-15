package org.raistlic.common.expectation;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import java.util.function.Function;

@RunWith(JUnit4.class)
public class BooleanExpectationPrimitiveTest {

  private static final Function<String, ? extends RuntimeException> EXCEPTION_PROVIDER = RuntimeException::new;

  @Test
  public void trueCandidateShouldPassIsTrueCheck() {

    new BooleanExpectation.Primitive(true, "name", EXCEPTION_PROVIDER).isTrue();
  }

  @Test(expected = RuntimeException.class)
  public void trueCandidateShouldFailIsFalseCheck() {

    new BooleanExpectation.Primitive(true, "name", EXCEPTION_PROVIDER).isFalse();
  }

  @Test
  public void trueCandidateShouldPassEqualToTrueCheck() {

    new BooleanExpectation.Primitive(true, "name", EXCEPTION_PROVIDER).isEqualTo(true);
  }

  @Test(expected = RuntimeException.class)
  public void trueCandidateShouldFailEqualToFalseCheck() {

    new BooleanExpectation.Primitive(true, "name", EXCEPTION_PROVIDER).isEqualTo(false);
  }

  @Test
  public void falseCandidateShouldPassIsFalseCheck() {

    new BooleanExpectation.Primitive(false, "name", EXCEPTION_PROVIDER).isFalse();
  }

  @Test(expected = RuntimeException.class)
  public void falseCandidateShouldFailIsTrueCheck() {

    new BooleanExpectation.Primitive(false, "name", EXCEPTION_PROVIDER).isTrue();
  }

  @Test
  public void falseCandidateShouldPassEqualToFalseCheck() {

    new BooleanExpectation.Primitive(false, "name", EXCEPTION_PROVIDER).isEqualTo(false);
  }

  @Test(expected = RuntimeException.class)
  public void falseCandidateShouldFailEqualToTrueCheck() {

    new BooleanExpectation.Primitive(false, "name", EXCEPTION_PROVIDER).isEqualTo(true);
  }
}
