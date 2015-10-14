package org.raistlic.common;

import org.raistlic.common.precondition.InvalidParameterException;

import java.util.WeakHashMap;

/**
 * A simple weak referenced thread local implementation, for the cases when we would like to reuse
 * some instances within one thread context as much as we can, but don't really care if it is GCed
 * every now and then and being re-created in such cases.
 *
 * @author Lei Chen (2015-10-14)
 */
public final class WeakThreadLocal<E> {

  public static WeakThreadLocal<StringBuilder> stringBuilder() {

    return StringBuilderHolder.INSTANCE;
  }

  private final WeakHashMap<Long, E> map;

  private final Factory<? extends E> factory;

  public WeakThreadLocal(Factory<? extends E> factory) {

    if (factory == null) {
      throw new InvalidParameterException("'factory' should not be null.");
    }

    this.factory = factory;
    this.map = new WeakHashMap<Long, E>();
  }

  public E get() {

    Long currentThreadId = Thread.currentThread().getId();
    E instance = map.get(currentThreadId);
    if (instance == null) {
      synchronized (map) {
        instance = map.get(currentThreadId);
        if (instance == null) {
          instance = factory.build();
          map.put(currentThreadId, instance);
        }
      }
    }
    return instance;
  }

  private static final class StringBuilderHolder {

    private static final WeakThreadLocal<StringBuilder> INSTANCE =
        new WeakThreadLocal<StringBuilder>(new Factory<StringBuilder>() {

          @Override
          public StringBuilder build() {

            return new StringBuilder();
          }

          @Override
          public boolean isReady() {

            return true;
          }
        });
  }
}
