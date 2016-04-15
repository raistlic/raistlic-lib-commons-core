package org.raistlic.common.expectation;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import java.util.function.Function;

@RunWith(JUnit4.class)
public class BooleanExpectationBoxedTest {

  private static final Function<String, ? extends RuntimeException> EXCEPTION_PROVIDER = RuntimeException::new;

  @Test
  public void trueCandidateShouldPassIsTrueCheck() {

    new BooleanExpectation.Boxed(Boolean.TRUE, "name", EXCEPTION_PROVIDER).isTrue();
  }

  @Test(expected = RuntimeException.class)
  public void trueCandidateShouldFailIsFalseCheck() {

    new BooleanExpectation.Boxed(Boolean.TRUE, "name", EXCEPTION_PROVIDER).isFalse();
  }

  @Test
  public void falseCandidateShouldPassIsFalseCheck() {

    new BooleanExpectation.Boxed(Boolean.FALSE, "name", EXCEPTION_PROVIDER).isFalse();
  }

  @Test(expected = RuntimeException.class)
  public void falseCandidateShouldFailIsTrueCheck() {

    new BooleanExpectation.Boxed(Boolean.FALSE, "name", EXCEPTION_PROVIDER).isTrue();
  }
}
