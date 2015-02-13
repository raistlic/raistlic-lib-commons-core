package org.raistlic.common;

/**
 * @author Lei.C (2015-02-09)
 */
public final class Numbers {

  public static int confine(int value, int min, int max) {

    value = Math.max(min, value);
    return Math.min(value, max);
  }

  public static long confine(long value, long min, long max) {

    value = Math.max(min, value);
    return Math.min(value, max);
  }

  private Numbers() { }
}
