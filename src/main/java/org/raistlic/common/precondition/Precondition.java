package org.raistlic.common.precondition;

import sun.awt.SunToolkit;

import javax.swing.SwingUtilities;

/**
 * @author lei.c
 * @since 2015-02-07
 */
public final class Precondition {

  public static void notNull(Object object) {

    if (object == null) {

      throw new NullParameterException();
    }
  }

  public static void inEventDispatchThread() {

    if (!SwingUtilities.isEventDispatchThread()) {

      throw new SunToolkit.IllegalThreadException("EDT violation.");
    }
  }

  private Precondition() { }
}
