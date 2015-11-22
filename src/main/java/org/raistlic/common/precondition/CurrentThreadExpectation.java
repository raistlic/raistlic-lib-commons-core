package org.raistlic.common.precondition;

import javax.swing.*;
import java.util.function.Function;

/**
 * @author lei.c (2015-11-19)
 */
public class CurrentThreadExpectation {

  private final Function<String, ? extends RuntimeException> exceptionProvider;

  public CurrentThreadExpectation(Function<String, ? extends RuntimeException> exceptionProvider) {

    if (exceptionProvider == null) {
      throw new InvalidParameterException("'exceptionProvider' cannot be null.");
    }

    this.exceptionProvider = exceptionProvider;
  }

  public void hasId(long id) {

    String message = "Current thread should have id " + id + ", but was " + Thread.currentThread().getId();
    hasId(id, message);
  }

  public void hasId(long id, String message) {

    if (Thread.currentThread().getId() != id) {
      throw exceptionProvider.apply(message);
    }
  }

  public void isEventDispatchingThread(String message) {

    if (! SwingUtilities.isEventDispatchThread()) {
      throw exceptionProvider.apply(message);
    }
  }

  public void isNotEventDispatchingThread(String message) {

    if (SwingUtilities.isEventDispatchThread()) {
      throw exceptionProvider.apply(message);
    }
  }
}
