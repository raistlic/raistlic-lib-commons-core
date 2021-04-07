package org.raistlic.common.util;

import java.util.Comparator;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.stream.Stream;

/**
 * @deprecated will be removed in 2.0, this one went too far.
 */
@Deprecated
public interface CustomStream<T, CS extends CustomStream<T, CS>> extends Stream<T> {

  @Override
  CS filter(Predicate<? super T> predicate);

  @Override
  CS distinct();

  @Override
  CS sorted();

  @Override
  CS sorted(Comparator<? super T> comparator);

  @Override
  CS peek(Consumer<? super T> action);

  @Override
  CS limit(long maxSize);

  @Override
  CS skip(long n);
}
