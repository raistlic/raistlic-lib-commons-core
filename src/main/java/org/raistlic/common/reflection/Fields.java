package org.raistlic.common.reflection;

import org.raistlic.common.precondition.Precondition;

import java.lang.reflect.Field;
import java.util.Arrays;
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
public final class Fields {

  public static Stream<Field> inClass(Class<?> inClass) {

    Precondition.param(inClass, "inClass").notNull();
    return Arrays.asList(inClass.getDeclaredFields()).stream();
  }

  public static Predicate<Field> predicateHasName(String name) {

    Precondition.param(name, "name").notNullOrEmpty();
    return new NamePredicate(name);
  }

  public static class FieldStream implements Stream<Field> {

    @Override
    public Stream<Field> filter(Predicate<? super Field> predicate) {

      throw new UnsupportedOperationException();
    }

    @Override
    public <R> Stream<R> map(Function<? super Field, ? extends R> mapper) {

      throw new UnsupportedOperationException();
    }

    @Override
    public IntStream mapToInt(ToIntFunction<? super Field> mapper) {

      throw new UnsupportedOperationException();
    }

    @Override
    public LongStream mapToLong(ToLongFunction<? super Field> mapper) {

      throw new UnsupportedOperationException();
    }

    @Override
    public DoubleStream mapToDouble(ToDoubleFunction<? super Field> mapper) {

      throw new UnsupportedOperationException();
    }

    @Override
    public <R> Stream<R> flatMap(Function<? super Field, ? extends Stream<? extends R>> mapper) {

      throw new UnsupportedOperationException();
    }

    @Override
    public IntStream flatMapToInt(Function<? super Field, ? extends IntStream> mapper) {

      throw new UnsupportedOperationException();
    }

    @Override
    public LongStream flatMapToLong(Function<? super Field, ? extends LongStream> mapper) {

      throw new UnsupportedOperationException();
    }

    @Override
    public DoubleStream flatMapToDouble(Function<? super Field, ? extends DoubleStream> mapper) {

      throw new UnsupportedOperationException();
    }

    @Override
    public Stream<Field> distinct() {

      throw new UnsupportedOperationException();
    }

    @Override
    public Stream<Field> sorted() {

      throw new UnsupportedOperationException();
    }

    @Override
    public Stream<Field> sorted(Comparator<? super Field> comparator) {

      throw new UnsupportedOperationException();
    }

    @Override
    public Stream<Field> peek(Consumer<? super Field> action) {

      throw new UnsupportedOperationException();
    }

    @Override
    public Stream<Field> limit(long maxSize) {

      throw new UnsupportedOperationException();
    }

    @Override
    public Stream<Field> skip(long n) {

      throw new UnsupportedOperationException();
    }

    @Override
    public void forEach(Consumer<? super Field> action) {

      throw new UnsupportedOperationException();
    }

    @Override
    public void forEachOrdered(Consumer<? super Field> action) {

      throw new UnsupportedOperationException();
    }

    @Override
    public Object[] toArray() {

      throw new UnsupportedOperationException();
    }

    @Override
    public <A> A[] toArray(IntFunction<A[]> generator) {

      throw new UnsupportedOperationException();
    }

    @Override
    public Field reduce(Field identity, BinaryOperator<Field> accumulator) {

      throw new UnsupportedOperationException();
    }

    @Override
    public Optional<Field> reduce(BinaryOperator<Field> accumulator) {

      throw new UnsupportedOperationException();
    }

    @Override
    public <U> U reduce(U identity, BiFunction<U, ? super Field, U> accumulator, BinaryOperator<U> combiner) {

      throw new UnsupportedOperationException();
    }

    @Override
    public <R> R collect(Supplier<R> supplier, BiConsumer<R, ? super Field> accumulator, BiConsumer<R, R> combiner) {

      throw new UnsupportedOperationException();
    }

    @Override
    public <R, A> R collect(Collector<? super Field, A, R> collector) {

      throw new UnsupportedOperationException();
    }

    @Override
    public Optional<Field> min(Comparator<? super Field> comparator) {

      throw new UnsupportedOperationException();
    }

    @Override
    public Optional<Field> max(Comparator<? super Field> comparator) {

      throw new UnsupportedOperationException();
    }

    @Override
    public long count() {

      throw new UnsupportedOperationException();
    }

    @Override
    public boolean anyMatch(Predicate<? super Field> predicate) {

      throw new UnsupportedOperationException();
    }

    @Override
    public boolean allMatch(Predicate<? super Field> predicate) {

      throw new UnsupportedOperationException();
    }

    @Override
    public boolean noneMatch(Predicate<? super Field> predicate) {

      throw new UnsupportedOperationException();
    }

    @Override
    public Optional<Field> findFirst() {

      throw new UnsupportedOperationException();
    }

    @Override
    public Optional<Field> findAny() {

      throw new UnsupportedOperationException();
    }

    @Override
    public Iterator<Field> iterator() {

      throw new UnsupportedOperationException();
    }

    @Override
    public Spliterator<Field> spliterator() {

      throw new UnsupportedOperationException();
    }

    @Override
    public boolean isParallel() {

      throw new UnsupportedOperationException();
    }

    @Override
    public Stream<Field> sequential() {

      throw new UnsupportedOperationException();
    }

    @Override
    public Stream<Field> parallel() {

      throw new UnsupportedOperationException();
    }

    @Override
    public Stream<Field> unordered() {

      throw new UnsupportedOperationException();
    }

    @Override
    public Stream<Field> onClose(Runnable closeHandler) {

      throw new UnsupportedOperationException();
    }

    @Override
    public void close() {

      throw new UnsupportedOperationException();
    }
  }

  private static final class NamePredicate implements Predicate<Field> {

    private final String name;

    private NamePredicate(String name) {

      this.name = name;
    }

    @Override
    public boolean test(Field field) {

      return field != null && field.getName().equals(this.name);
    }
  }

  private Fields() { }
}
