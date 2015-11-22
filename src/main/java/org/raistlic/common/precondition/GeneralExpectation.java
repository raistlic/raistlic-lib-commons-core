package org.raistlic.common.precondition;

import org.raistlic.common.predicate.Predicates;

import java.util.function.Function;
import java.util.function.Predicate;

/**
 * The class helps to check a specified {@code candidate} with certain expectations, and throws
 * a custom runtime exception when the check fails.
 *
 * @author Lei Chen (2015-10-14)
 */
public class GeneralExpectation<E> extends AbstractExpectation<E> {

  private final String name;

  public GeneralExpectation(E candidate,
                            String name,
                            Function<String, ? extends RuntimeException> exceptionProvider) {

    super(candidate, exceptionProvider);
    this.name = name;
  }

  String name() {

    return name;
  }

  /**
   * The method claims that the {@code candidate} should be {@code null}, and will throw exception
   * if it is not.
   */
  public void isNull() {

    String message = "";
    if (name != null) {
      message = "'" + name + "' should be null, but was " + super.getCandidate();
    }
    isNull(message);
  }

  /**
   * The method claims that the {@code candidate} should be {@code null}, and will throw exception
   * with the specified {@code message} if it is not.
   *
   * @param message the message to be thrown with the exception, in case the check fails.
   */
  public void isNull(String message) {

    setMessage(message);
    setPredicate(Predicates.isNull());
    evaluate();
  }

  public void notNull() {

    String message = "";
    if (name != null) {
      message = "'" + name + "' should not be null.";
    }
    notNull(message);
  }

  public void notNull(String message) {

    setMessage(message);
    setPredicate(Predicates.notNull());
    evaluate();
  }

  public void equalTo(E target) {

    String message = "";
    if (name != null) {
      message = "'" + name + "' should be equal to '" + target + "', but is not.";
    }
    equalTo(target, message);
  }

  public void equalTo(E target, String message) {

    setMessage(message);
    setPredicate(Predicates.equalTo(target));
    evaluate();
  }

  public void notEqualTo(E target) {

    String message = "";
    if (name != null) {
      message = "'" + name + "' should not be equal to '" + target + "'.";
    }
    notEqualTo(target, message);
  }

  public void notEqualTo(E target, String message) {

    setMessage(message);
    setPredicate(Predicates.notEqualTo(target));
    evaluate();
  }

  public void matches(Predicate<? super E> predicate) {

    String message = "";
    if (name != null) {
      message = "'" + name + "' does not match the specified predicate.";
    }
    matches(predicate, message);
  }

  public void matches(Predicate<? super E> predicate, String message) {

    Precondition.param(predicate, "predicate").notNull();

    setMessage(message);
    setPredicate(predicate);
    evaluate();
  }
}
