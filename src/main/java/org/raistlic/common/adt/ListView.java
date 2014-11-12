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

/**
 * This interface does not extends {@link java.util.Collection}, because java's
 * collection interface is essentially a "mutable collection" interface, while
 * this interface tries to define a read-only list view.
 * <p/>
 * <p/>
 * Design ideas from {@link java.util.Collection}.
 *
 * @author Lei.C
 */
public interface ListView<E> extends Iterable<E> {

  public int size();

  public E get(int index);

  public boolean isEmpty();

  public boolean contains(E element);
}
