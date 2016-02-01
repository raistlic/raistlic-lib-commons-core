package org.raistlic.common.adt;

import org.raistlic.common.precondition.Precondition;
import org.raistlic.common.predicate.Predicates;

import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.function.Predicate;

/**
 * @author lei.c (2015-12-22)
 */
public abstract class FilteredList<E> implements Iterable<E> {

  @SuppressWarnings("unchecked")
  public static <E> FilteredList<E> of(E[] elements, Predicate<? super E> predicate) {

    Precondition.param(elements, "elements").notNull();
    Precondition.param(predicate, "predicate").notNull();

    if (elements.length == 0) {
      return (FilteredList<E>) EMPTY;
    }

    BitMap.Builder builder = BitMap.builder(elements.length + 1);
    for (int i = 0, length = elements.length; i < length; i++) {
      if (predicate.test(elements[i])) {
        builder.set(i);
      }
    }
    return new ArrayWrapper<>(elements, builder.get());
  }

  @SuppressWarnings("unchecked")
  public static <E> FilteredList<E> of(List<E> elements, Predicate<? super E> predicate) {

    Precondition.param(elements, "elements").notNull();
    Precondition.param(predicate, "predicate").notNull();

    if (elements.isEmpty()) {
      return (FilteredList<E>) EMPTY;
    }

    BitMap bitMap = BitMap.newInstance(elements, predicate);
    return new ListWrapper<>(elements, bitMap);
  }

  private FilteredList() { }

  public abstract int size();

  public abstract E get(int index);

  public final boolean isEmpty() {

    return size() == 0;
  }

  @Override
  public Iterator<E> iterator() {

    return this.new IndexIterator();
  }

  @SuppressWarnings("rawtypes")
  private static final FilteredList EMPTY = new FilteredList() {

    @Override
    public int size() {

      return 0;
    }

    @Override
    public Object get(int index) {

      Precondition.param(index).matches(Predicates.dummyFalse(), "Index out of bounds: " + index);
      return null; // never reaches here.
    }
  };

  private static final class ArrayWrapper<E> extends FilteredList<E> {

    private E[] array;

    private BitMap bitMap;

    private ArrayWrapper(E[] array, BitMap bitMap) {

      this.array = array;
      this.bitMap = bitMap;
    }

    @Override
    public int size() {

      return bitMap.rankOne(array.length);
    }

    @Override
    public E get(int index) {

      Precondition.param(index).noLessThan(0, "Index out of bounds: " + index);
      Precondition.param(index).lessThan(size(), "Index out of bounds: " + index);

      index = bitMap.selectOne(index);
      return array[index];
    }
  }

  private static final class ListWrapper<E> extends FilteredList<E> {

    private List<E> list;

    private BitMap bitMap;

    private ListWrapper(List<E> list, BitMap bitMap) {

      this.list = list;
      this.bitMap = bitMap;
    }

    @Override
    public int size() {

      return bitMap.rankOne(list.size());
    }

    @Override
    public E get(int index) {

      Precondition.param(index).noLessThan(0, "Index out of bounds: " + index);
      Precondition.param(index).lessThan(size(), "Index out of bounds: " + index);

      index = bitMap.selectOne(index);
      return list.get(index);
    }
  }

  private class IndexIterator implements Iterator<E> {

    private int index = 0;

    @Override
    public boolean hasNext() {

      return index < size();
    }

    @Override
    public E next() {

      if (!hasNext()) {
        throw new NoSuchElementException("No next element.");
      }

      E next = get(index);
      index++;
      return next;
    }
  }
}
