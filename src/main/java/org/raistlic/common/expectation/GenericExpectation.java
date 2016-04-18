package org.raistlic.common.expectation;

import java.util.function.Function;

public class GenericExpectation<E> extends AbstractGenericExpectation<E, GenericExpectation<E>> {

  GenericExpectation(E candidate, String name, Function<String, ? extends RuntimeException> exceptionProvider) {

    super(candidate, name, exceptionProvider);
  }
}
