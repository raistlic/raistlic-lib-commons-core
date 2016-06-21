package org.raistlic.common.expectation;

import java.util.regex.Pattern;

public interface StringExpectation extends Expectation<String, StringExpectation> {

  StringExpectation isEmpty();

  StringExpectation isEmpty(String message);

  StringExpectation isNotEmpty();

  StringExpectation isNotEmpty(String message);

  StringExpectation isNullOrEmpty();

  StringExpectation isNullOrEmpty(String message);

  StringExpectation isNotNullOrEmpty();

  StringExpectation isNotNullOrEmpty(String message);

  StringExpectation hasLength(int length);

  StringExpectation hasLength(int length, String message);

  StringExpectation matchesPattern(Pattern pattern);

  StringExpectation matchesPattern(Pattern pattern, String message);

  StringExpectation matchesPattern(String pattern);

  StringExpectation matchesPattern(String pattern, String message);
}
