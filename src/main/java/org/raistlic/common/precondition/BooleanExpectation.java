package org.raistlic.common.precondition;

import java.util.function.Function;
import java.util.function.Predicate;

/**
 * The class defines both expectation class for the primitive and boxed boolean values.
 *
 * @author Lei CHEN (2015-11-20)
 */
public final class BooleanExpectation {

  public static class Primitive {

    private final boolean candidate;

    private final String name;

    private final Function<String, ? extends RuntimeException> exceptionProvider;

    Primitive(boolean candidate,
              String name,
              Function<String, ? extends RuntimeException> exceptionProvider) {

      this.candidate = candidate;
      this.name = name;
      this.exceptionProvider = exceptionProvider;
    }

    public void isTrue() {

      String message = "";
      if (name != null) {
        message = name + " should be true, but was false.";
      }
      isTrue(message);
    }

    public void isTrue(String message) {

      if (!candidate) {
        throw exceptionProvider.apply(message);
      }
    }

    public void isFalse() {

      String message = "";
      if (name != null) {
        message = name + " should be false, but was true.";
      }
      isFalse(message);
    }

    public void isFalse(String message) {

      if (candidate) {
        throw exceptionProvider.apply(message);
      }
    }
  }

  public static class Boxed extends AbstractExpectation<Boolean> {

    private final String name;

    Boxed(Boolean candidate,
          String name,
          Function<String, ? extends RuntimeException> exceptionProvider) {

      super(candidate, exceptionProvider);

      this.name = name;
    }

    public void isTrue() {

      String message = "";
      if (name != null) {
        message = name + " should be true, but was " + getCandidate();
      }
      isTrue(message);
    }

    public void isTrue(String message) {

      super.setMessage(message);
      super.setPredicate(BooleanPredicate.TRUE);
      super.evaluate();
    }

    public void isFalse() {

      String message = "";
      if (name != null) {
        message = name + " should be false, but was " + getCandidate();
      }
      isFalse(message);
    }

    public void isFalse(String message) {

      super.setMessage(message);
      super.setPredicate(BooleanPredicate.FALSE);
      super.evaluate();
    }
  }

  private enum BooleanPredicate implements Predicate<Boolean> {

    TRUE(true),
    FALSE(false);

    private final boolean expected;

    BooleanPredicate(boolean expected) {

      this.expected = expected;
    }

    @Override
    public boolean test(Boolean aBoolean) {

      return aBoolean != null && aBoolean == expected;
    }
  }

  private BooleanExpectation() {

  }
}
