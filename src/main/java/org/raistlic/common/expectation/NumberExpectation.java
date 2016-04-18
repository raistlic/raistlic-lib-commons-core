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

import org.raistlic.common.precondition.InvalidParameterException;
import org.raistlic.common.predicate.NumberPredicates;

import java.util.function.Function;

/**
 * @author Lei Chen (2015-10-16)
 */
public class NumberExpectation<N extends Number & Comparable<N>> extends AbstractGenericExpectation<N, NumberExpectation<N>> {

  public NumberExpectation(N candidate,
                           String name,
                           Function<String, ? extends RuntimeException> exceptionProvider) {

    super(candidate, name, exceptionProvider);

    if (exceptionProvider == null) {
      throw new InvalidParameterException("'exceptionProvider' should not be null.");
    }
  }

  public NumberExpectation<N> greaterThan(N reference) {

    if (reference == null) {
      throw new InvalidParameterException("'reference' should not be null.");
    }

    String message = "";
    if (name() != null) {
      message = "'" + name() + "' should be greater than " + reference +
          " but is not (actual value: " + getCandidate() + ")";
    }
    return greaterThan(reference, message);
  }

  public NumberExpectation<N> greaterThan(N reference, String message) {

    if (reference == null) {
      throw new InvalidParameterException("'reference' should not be null.");
    }

    setMessage(message);
    setPredicate(NumberPredicates.greaterThan(reference));
    return evaluate();
  }

  public NumberExpectation<N> greaterThanOrEqualTo(N reference) {

    if (reference == null) {
      throw new InvalidParameterException("'reference' should not be null.");
    }

    String message = "";
    if (name() != null) {

      message = "'" + name() + "' should be greater than or equal to " + reference +
          ", but is not (actual value: " + getCandidate() + ")";
    }
    return greaterThanOrEqualTo(reference, message);
  }

  public NumberExpectation<N> greaterThanOrEqualTo(N reference, String message) {

    if (reference == null) {
      throw new InvalidParameterException("'reference' should not be null.");
    }

    setMessage(message);
    setPredicate(NumberPredicates.greaterThanOrEqualTo(reference));
    return evaluate();
  }

  public NumberExpectation<N> noLessThan(N reference) {

    return greaterThanOrEqualTo(reference);
  }

  public NumberExpectation<N> noLessThan(N reference, String message) {

    return greaterThanOrEqualTo(reference, message);
  }

  public NumberExpectation<N> lessThan(N reference) {

    if (reference == null) {
      throw new InvalidParameterException("'reference' should not be null.");
    }

    String message = "";
    if (name() != null) {
      message = "'" + name() + "' should be less than " + reference +
          ", but is not (actual value: " + getCandidate() + ")";
    }
    return lessThan(reference, message);
  }

  public NumberExpectation<N> lessThan(N reference, String message) {

    if (reference == null) {
      throw new InvalidParameterException("'reference' should not be null.");
    }

    setMessage(message);
    setPredicate(NumberPredicates.lessThan(reference));
    return evaluate();
  }

  public NumberExpectation<N> lessThanOrEqualTo(N reference) {

    if (reference == null) {
      throw new InvalidParameterException("'reference' should not be null.");
    }

    String message = "";
    if (name() != null) {
      message = "'" + name() + "' should be less than or equal to " + reference +
          ", but is not (actual value: " + getCandidate() + ")";
    }
    return lessThanOrEqualTo(reference, message);
  }

  public NumberExpectation<N> lessThanOrEqualTo(N reference, String message) {

    if (reference == null) {
      throw new InvalidParameterException("'reference' should not be null.");
    }

    setMessage(message);
    setPredicate(NumberPredicates.lessThanOrEqualTo(reference));
    return evaluate();
  }

  public NumberExpectation<N> noGreaterThan(N reference) {

    return lessThanOrEqualTo(reference);
  }

  public NumberExpectation<N> noGreaterThan(N reference, String message) {

    return lessThanOrEqualTo(reference, message);
  }
}
