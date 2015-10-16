package org.raistlic.common.util;

import org.raistlic.common.Factory;

/**
 * @author Lei Chen (2015-10-14)
 */
public interface ExceptionFactory<E extends RuntimeException> extends Factory<E> {

  E build(String message, Throwable cause);
}
