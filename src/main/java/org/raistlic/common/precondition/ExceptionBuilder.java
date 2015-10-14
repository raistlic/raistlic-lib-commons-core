package org.raistlic.common.precondition;

import org.raistlic.common.Factory;

/**
 * @author Lei Chen (2015-10-14)
 */
public interface ExceptionBuilder<E extends RuntimeException> extends Factory<E> {

  ExceptionBuilder<E> withMessage(String message);

  ExceptionBuilder<E> withCause(Throwable cause);
}
