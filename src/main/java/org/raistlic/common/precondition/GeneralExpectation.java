package org.raistlic.common.precondition;

import java.util.function.Predicate;

/**
 * @author Lei Chen (2015-10-14)
 */
public class GeneralExpectation<E> extends AbstractExpectation<E, GeneralExpectation<E>> {

  private final String name;

  GeneralExpectation(E candidate, ExceptionBuilder<?> exceptionBuilder, String name) {

    super(candidate, exceptionBuilder);
    this.name = name;
  }

  public GeneralExpectation<E> isNull() {

    String message = "";
    if (name != null) {
      message = "'" + name + "' should be null, but was " + super.getCandidate();
    }
    return isNull(message);
  }

  public GeneralExpectation<E> isNull(String message) {

    return withMessage(message).withPredicate(ObjectPredicates.IS_NULL).evaluate();
  }

  public GeneralExpectation<E> notNull() {

    String message = "";
    if (name != null) {
      message = "'" + name + "' should not be null.";
    }
    return notNull(message);
  }

  public GeneralExpectation<E> notNull(String message) {

    return withMessage(message).withPredicate(ObjectPredicates.NOT_NULL).evaluate();
  }

  public GeneralExpectation<E> equalTo(E target) {

    String message = "";
    if (name != null) {
      message = "'" + name + "' should be equal to '" + target + "', but is not.";
    }
    return equalTo(target, message);
  }

  public GeneralExpectation<E> equalTo(E target, String message) {

    return withMessage(message).withPredicate(ObjectPredicates.equalsObject(target)).evaluate();
  }

  public GeneralExpectation<E> notEqualTo(E target) {

    String message = "";
    if (name != null) {
      message = "'" + name + "' should not be equal to '" + target + "'.";
    }
    return notEqualTo(target, message);
  }

  public GeneralExpectation<E> notEqualTo(E target, String message) {

    return withMessage(message).withPredicate(ObjectPredicates.notEqualsObject(target)).evaluate();
  }

  public GeneralExpectation<E> matches(Predicate<? super E> predicate) {

    String message = "";
    if (name != null) {
      message = "'" + name + "' is invalid.";
    }
    return matches(predicate, message);
  }

  public GeneralExpectation<E> matches(Predicate<? super E> predicate, String message) {

    Precondition.param(predicate, "predicate").notNull();

    return withMessage(message).withPredicate(predicate).evaluate();
  }
}
