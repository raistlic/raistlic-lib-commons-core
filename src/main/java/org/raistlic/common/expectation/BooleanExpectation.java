/*
 * Copyright 2015 Lei CHEN (raistlic@gmail.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.raistlic.common.expectation;

import java.util.function.Function;
import java.util.function.Predicate;

/**
 * The class defines both expectation class for the primitive and boxed boolean values.
 *
 * @author Lei CHEN (2015-11-20)
 */
public final class BooleanExpectation {

  /**
   * The class defines assertion methods for primitive {@code boolean} values.
   */
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

    public void isEqualTo(boolean expected) {

      String message = "";
      if (name != null) {
        message = "'" + name + "' should be " + expected + ", but was " + candidate;
      }
      isEqualTo(expected, message);
    }

    public void isEqualTo(boolean expected, String message) {

      if (candidate != expected) {
        throw exceptionProvider.apply(message);
      }
    }
  }

  /**
   * The class defines assertion methods for boxed {@link Boolean} values.
   */
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
