/*
 * Copyright 2013 Lei CHEN (raistlic@gmail.com)
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

import java.lang.ref.WeakReference;
import java.util.Iterator;

/**
 * This class simply encapsulates an array to be used as a cache, which holds
 * weak references.
 * 
 * It is intended to store "not so important" contents, which can be GCed
 * when required.
 * 
 * The iterator of this class can be used to iterate it, or remove elements 
 * from it, removing elements using the iterator will not effect the subsequent
 * elements' positions.
 *
 * @author Lei.C
 */
public class WeakArray<E> implements Iterable<E> {

  private WeakReference<E>[] data;

  @SuppressWarnings({"unchecked", "rawtypes"})
  public WeakArray(int size) {

    if( size < 0 )
      throw new IllegalArgumentException("Invalid array size: " + size);

    data = (WeakReference<E>[])new WeakReference[size];
  }

  public int length() {
    
    return data.length;
  }
  
  public void set(int index, E element) {
    
    // potentially throws ArrayIndexOutOfBoundsException
    if( get(index) != element )
      data[index] = new WeakReference<E>(element);
  }
  
  public E get(int index) {
    
    // potentially throws ArrayIndexOutOfBoundsException
    WeakReference<E> ref = data[index];
    return ref == null ? null : ref.get();
  }
  
  public void remove(int index) {
    
    // potentially throws ArrayIndexOutOfBoundsException
    data[index] = null;
  }

  @Override
  public Iterator<E> iterator() {
    
    return new IndexIterator();
  }
  
  private class IndexIterator implements Iterator<E> {
    
    private int index = 0;

    @Override
    public boolean hasNext() {
      
      return index < length();
    }

    @Override
    public E next() {
      
      // potentially throws ArrayIndexOutOfBoundsException
      E result = get(index);
      index++;
      return result;
    }

    @Override
    public void remove() {
      
      // potentially throws ArrayIndexOutOfBoundsException
      WeakArray.this.remove(index);
      index++;
    }
  }
}
