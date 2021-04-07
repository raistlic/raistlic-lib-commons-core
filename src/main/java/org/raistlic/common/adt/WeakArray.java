/*
 * Copyright 2015 Lei CHEN (raistlic@gmail.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.raistlic.common.adt;

import org.raistlic.common.precondition.Param;

import java.lang.ref.WeakReference;
import java.util.*;

/**
 * This class simply encapsulates an array to be used as a cache, which holds
 * weak references.
 * <p>
 * It is intended to store "not so important" contents, which can be GCed
 * when required.
 * <p>
 * The iterator of this class can be used to iterate it, or remove elements
 * from it, removing elements using the iterator will not effect the subsequent
 * elements' positions.
 */
public final class WeakArray<E> implements Iterable<E> {

  private final int size;

  private final Map<Integer, WeakReference<E>> data;

  public WeakArray(int size) {

    Param.isTrue(size >= 0, "size cannot be less than 0");

    this.size = size;
    this.data = new HashMap<>(size * 2);
  }

  public int length() {

    return size;
  }

  public E set(int index, E element) {

    Param.isTrue(index >= 0, "index cannot be less than 0");
    Param.isTrue(index < size, "index must be less than array size");

    E existing = get(index);
    data.put(index, new WeakReference<>(element));
    return existing;
  }

  public E get(int index) {

    Param.isTrue(index >= 0, "index cannot be less than 0");
    Param.isTrue(index < size, "index must be less than array size");

    return Optional.ofNullable(data.get(index))
      .map(WeakReference::get)
      .orElse(null);
  }

  public E remove(int index) {

    return set(index, null);
  }

  @Override
  public Iterator<E> iterator() {

    return new IndexIterator();
  }

  private class IndexIterator implements Iterator<E> {

    private int index = -1;

    private boolean removed = false;

    @Override
    public boolean hasNext() {

      return index + 1 < size;
    }

    @Override
    public E next() {

      if (!hasNext()) {
        throw new NoSuchElementException();
      }

      E result = get(index + 1);
      index++;
      removed = false;
      return result;
    }

    @Override
    public void remove() {

      if (index < 0) {
        throw new IllegalStateException("next is not called");
      }
      if (removed) {
        throw new IllegalStateException("current element is already removed");
      }
      if (index >= size) {
        throw new NoSuchElementException();
      }

      WeakArray.this.remove(index);
      removed = true;
    }
  }
}
