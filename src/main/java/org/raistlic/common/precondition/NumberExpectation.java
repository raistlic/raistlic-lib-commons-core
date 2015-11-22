package org.raistlic.common.precondition;

import org.raistlic.common.predicate.NumberPredicates;

import java.util.function.Function;

/**
 * @author Lei Chen (2015-10-16)
 */
public class NumberExpectation<N extends Number & Comparable<N>> extends GeneralExpectation<N> {

  public NumberExpectation(N candidate,
                           String name,
                           Function<String, ? extends RuntimeException> exceptionProvider) {

    super(candidate, name, exceptionProvider);

    if (exceptionProvider == null) {
      throw new InvalidParameterException("'exceptionProvider' should not be null.");
    }
  }

  public void greaterThan(N reference) {

    if (reference == null) {
      throw new InvalidParameterException("'reference' should not be null.");
    }

    String message = "";
    if (name() != null) {
      message = "'" + name() + "' should be greater than " + reference +
          " but is not (actual value: " + getCandidate() + ")";
    }
    greaterThan(reference, message);
  }

  public void greaterThan(N reference, String message) {

    if (reference == null) {
      throw new InvalidParameterException("'reference' should not be null.");
    }

    setMessage(message);
    setPredicate(NumberPredicates.greaterThan(reference));
    evaluate();
  }

  public void greaterThanOrEqualTo(N reference) {

    if (reference == null) {
      throw new InvalidParameterException("'reference' should not be null.");
    }

    String message = "";
    if (name() != null) {

      message = "'" + name() + "' should be greater than or equal to " + reference +
          ", but is not (actual value: " + getCandidate() + ")";
    }
    greaterThanOrEqualTo(reference, message);
  }

  public void greaterThanOrEqualTo(N reference, String message) {

    if (reference == null) {
      throw new InvalidParameterException("'reference' should not be null.");
    }

    setMessage(message);
    setPredicate(NumberPredicates.greaterThanOrEqualTo(reference));
    evaluate();
  }

  public void noLessThan(N reference) {

    greaterThanOrEqualTo(reference);
  }

  public void noLessThan(N reference, String message) {

    greaterThanOrEqualTo(reference, message);
  }

  public void lessThan(N reference) {

    if (reference == null) {
      throw new InvalidParameterException("'reference' should not be null.");
    }

    String message = "";
    if (name() != null) {
      message = "'" + name() + "' should be less than " + reference +
          ", but is not (actual value: " + getCandidate() + ")";
    }
    lessThan(reference, message);
  }

  public void lessThan(N reference, String message) {

    if (reference == null) {
      throw new InvalidParameterException("'reference' should not be null.");
    }

    setMessage(message);
    setPredicate(NumberPredicates.lessThan(reference));
    evaluate();
  }

  public void lessThanOrEqualTo(N reference) {

    if (reference == null) {
      throw new InvalidParameterException("'reference' should not be null.");
    }

    String message = "";
    if (name() != null) {
      message = "'" + name() + "' should be less than or equal to " + reference +
          ", but is not (actual value: " + getCandidate() + ")";
    }
    lessThanOrEqualTo(reference, message);
  }

  public void lessThanOrEqualTo(N reference, String message) {

    if (reference == null) {
      throw new InvalidParameterException("'reference' should not be null.");
    }

    setMessage(message);
    setPredicate(NumberPredicates.lessThanOrEqualTo(reference));
    evaluate();
  }

  public void noGreaterThan(N reference) {

    lessThanOrEqualTo(reference);
  }

  public void noGreaterThan(N reference, String message) {

    lessThanOrEqualTo(reference, message);
  }
}
