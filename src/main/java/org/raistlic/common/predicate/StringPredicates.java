package org.raistlic.common.predicate;

import org.raistlic.common.precondition.InvalidParameterException;

import java.util.function.Predicate;
import java.util.regex.Pattern;

/**
 * @author Lei Chen (2015-10-14)
 */
public final class StringPredicates {

  public static Predicate<String> isEmpty() {

    return StringIsEmptyPredicate.INSTANCE;
  }

  public static Predicate<String> notEmpty() {

    return Predicates.not(isEmpty());
  }

  public static Predicate<String> hasLength(int length) {

    throw new UnsupportedOperationException();
  }

  public static Predicate<String> matchesPattern(Pattern pattern) {

    if (pattern == null) {
      throw new InvalidParameterException("'pattern' should not be null.");
    }
    return new StringMatchesPatternPredicate(pattern);
  }

  private enum StringIsEmptyPredicate implements Predicate<String> {

    INSTANCE;

    @Override
    public boolean test(String s) {

      return "".equals(s);
    }
  }

  private class StringLengthPredicate implements Predicate<String> {

    private final int length;

    public StringLengthPredicate(int length) {

      this.length = length;
    }

    @Override
    public boolean test(String s) {

      return s != null && s.length() == length;
    }
  }

  private static final class StringMatchesPatternPredicate implements Predicate<String> {

    private final Pattern pattern;

    public StringMatchesPatternPredicate(Pattern pattern) {

      this.pattern = pattern;
    }

    @Override
    public boolean test(String s) {

      return s != null && pattern.matcher(s).matches();
    }
  }

  private StringPredicates() { }
}
