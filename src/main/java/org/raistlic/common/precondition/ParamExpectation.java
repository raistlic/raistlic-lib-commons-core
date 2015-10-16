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

package org.raistlic.common.precondition;

/**
 * @author Lei CHEN (2015-02-18)
 * @since 1.2
 */
public final class ParamExpectation {

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
        message = (name == null) ? "" : "'" + name + "' should be less than " + target +
                ", but is not (" + evaluative + ").";
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
        message = (name == null) ? "" : "'" + name + "' should be no less than " + target +
                ", but is not (" + evaluative + ").";
      }
      error(message);
    }

    public void greaterThan(int target) {

      greaterThan(target, null);
    }

    public void greaterThan(int target, String message) {

      if (evaluative > target) {
        return;
      }
      if (message == null) {
        message = (name == null) ? "" : "'" + name + "' should be greater than " + target +
                ", but is not (" + evaluative + ").";
      }
      error(message);
    }

    public void noGreaterThan(int target) {

      noGreaterThan(target, null);
    }

    public void noGreaterThan(int target, String message) {

      if (evaluative <= target) {
        return;
      }
      if (message == null) {
        message = (name == null) ? "" : "'" + name + "' should be no greater than " + target +
                ", but is not (" + evaluative + ").";
      }
      error(message);
    }
  }

  private static void error(String message) {

    throw new InvalidParameterException(message);
  }
}
