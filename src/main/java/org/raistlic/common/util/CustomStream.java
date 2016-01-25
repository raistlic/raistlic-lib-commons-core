package org.raistlic.common.util;

import java.util.Comparator;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.stream.Stream;

/**
 * @author Lei Chen (2016-01-25)
 */
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
