package org.raistlic.common.util;

import org.raistlic.common.precondition.Precondition;

import java.util.Comparator;
import java.util.Iterator;
import java.util.Optional;
import java.util.Spliterator;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.BinaryOperator;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.function.ToDoubleFunction;
import java.util.function.ToIntFunction;
import java.util.function.ToLongFunction;
import java.util.stream.Collector;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;
import java.util.stream.LongStream;
import java.util.stream.Stream;

/**
 * @author Lei Chen (2016-01-07)
 */
public abstract class CustomStreamAdapter<T, CS extends CustomStreamAdapter<T, CS>> implements CustomStream<T, CS> {

  private Stream<T> originalStream;

  protected CustomStreamAdapter(Stream<T> originalStream) {

    Precondition.param(originalStream, "originalStream").notNull();
    this.originalStream = originalStream;
  }

  @SuppressWarnings("unchecked")
  protected final CS getThis() {
    return (CS) this;
  }

  @Override
  public CS filter(Predicate<? super T> predicate) {

    originalStream.filter(predicate);
    return getThis();
  }

  @Override
  public <R> Stream<R> map(Function<? super T, ? extends R> mapper) {

    return originalStream.map(mapper);
  }

  @Override
  public IntStream mapToInt(ToIntFunction<? super T> mapper) {

    return originalStream.mapToInt(mapper);
  }

  @Override
  public LongStream mapToLong(ToLongFunction<? super T> mapper) {

    return originalStream.mapToLong(mapper);
  }

  @Override
  public DoubleStream mapToDouble(ToDoubleFunction<? super T> mapper) {

    return originalStream.mapToDouble(mapper);
  }

  @Override
  public <R> Stream<R> flatMap(Function<? super T, ? extends Stream<? extends R>> mapper) {

    return originalStream.flatMap(mapper);
  }

  @Override
  public IntStream flatMapToInt(Function<? super T, ? extends IntStream> mapper) {

    return originalStream.flatMapToInt(mapper);
  }

  @Override
  public LongStream flatMapToLong(Function<? super T, ? extends LongStream> mapper) {

    return originalStream.flatMapToLong(mapper);
  }

  @Override
  public DoubleStream flatMapToDouble(Function<? super T, ? extends DoubleStream> mapper) {

    return originalStream.flatMapToDouble(mapper);
  }

  @Override
  public CS distinct() {

    originalStream = originalStream.distinct();
    return getThis();
  }

  @Override
  public CS sorted() {

    originalStream = originalStream.sorted();
    return getThis();
  }

  @Override
  public CS sorted(Comparator<? super T> comparator) {

    originalStream = originalStream.sorted(comparator);
    return getThis();
  }

  @Override
  public CS peek(Consumer<? super T> action) {

    originalStream = originalStream.peek(action);
    return getThis();
  }

  @Override
  public CS limit(long maxSize) {

    originalStream = originalStream.limit(maxSize);
    return getThis();
  }

  @Override
  public CS skip(long n) {

    originalStream = originalStream.skip(n);
    return getThis();
  }

  @Override
  public void forEach(Consumer<? super T> action) {

    originalStream.forEach(action);
  }

  @Override
  public void forEachOrdered(Consumer<? super T> action) {

    originalStream.forEachOrdered(action);
  }

  @Override
  public Object[] toArray() {

    return originalStream.toArray();
  }

  @Override
  public <A> A[] toArray(IntFunction<A[]> generator) {

    return originalStream.toArray(generator);
  }

  @Override
  public T reduce(T identity, BinaryOperator<T> accumulator) {

    return originalStream.reduce(identity, accumulator);
  }

  @Override
  public Optional<T> reduce(BinaryOperator<T> accumulator) {

    return originalStream.reduce(accumulator);
  }

  @Override
  public <U> U reduce(U identity, BiFunction<U, ? super T, U> accumulator, BinaryOperator<U> combiner) {

    return originalStream.reduce(identity, accumulator, combiner);
  }

  @Override
  public <R> R collect(Supplier<R> supplier, BiConsumer<R, ? super T> accumulator, BiConsumer<R, R> combiner) {

    return originalStream.collect(supplier, accumulator, combiner);
  }

  @Override
  public <R, A> R collect(Collector<? super T, A, R> collector) {

    return originalStream.collect(collector);
  }

  @Override
  public Optional<T> min(Comparator<? super T> comparator) {

    return originalStream.min(comparator);
  }

  @Override
  public Optional<T> max(Comparator<? super T> comparator) {

    return originalStream.max(comparator);
  }

  @Override
  public long count() {

    return originalStream.count();
  }

  @Override
  public boolean anyMatch(Predicate<? super T> predicate) {

    return originalStream.anyMatch(predicate);
  }

  @Override
  public boolean allMatch(Predicate<? super T> predicate) {

    return originalStream.allMatch(predicate);
  }

  @Override
  public boolean noneMatch(Predicate<? super T> predicate) {

    return originalStream.noneMatch(predicate);
  }

  @Override
  public Optional<T> findFirst() {

    return originalStream.findFirst();
  }

  @Override
  public Optional<T> findAny() {

    return originalStream.findAny();
  }

  @Override
  public Iterator<T> iterator() {

    return originalStream.iterator();
  }

  @Override
  public Spliterator<T> spliterator() {

    return originalStream.spliterator();
  }

  @Override
  public boolean isParallel() {

    return originalStream.isParallel();
  }

  @Override
  public CS sequential() {

    originalStream = originalStream.sequential();
    return getThis();
  }

  @Override
  public CS parallel() {

    originalStream = originalStream.parallel();
    return getThis();
  }

  @Override
  public CS unordered() {

    originalStream = originalStream.unordered();
    return getThis();
  }

  @Override
  public CS onClose(Runnable closeHandler) {

    originalStream = originalStream.onClose(closeHandler);
    return getThis();
  }

  @Override
  public void close() {

    originalStream.close();
  }
}
