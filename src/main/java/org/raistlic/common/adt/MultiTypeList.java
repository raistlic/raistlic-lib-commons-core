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

import java.util.ArrayList;
import java.util.List;

/**
 * This class is designed to replace the usage of {@link javax.swing.event.EventListenerList},
 * because the swing EventListenerList requires a listener to extend 
 * {@link java.util.EventListener}, which is a not-necessary hurdle.
 * 
 * <p/>
 * For example, the following instance can replace a {@link javax.swing.event.EventListenerList}:
 * 
 * <pre>
 * {@code 
 * // EventListenerList listenerList = new EventListenerList();
 * MultiTypeList<EventListener> listenerList = MultiTypeList.newInstance(EventListener.class);
 * 
 * // listenerList.add(ActionListener.class, listener);
 * listenerList.add(ActionListener.class, listener);
 * 
 * // listenerList.remove(ActionListener.class, listener);
 * listenerList.remove(ActionListener.class, listener);
 * 
 * // for(ActionListener l : listenerList.getListeners(ActionListener.class)) { ... }
 * for(ActionListener l : listenerList.getList(ActionListener.class)) { ... }
 * }
 * </pre>
 * 
 * <p/>
 * There is no reason why it cannot be used in other cases, rather than maintaining 
 * listeners in Swing applications.
 *
 * @param <B> the referenced base type of the list.
 *
 * @author Lei.C
 */
public class MultiTypeList<B> {
  
  public static <E> MultiTypeList<E> newInstance() {
    
    return new MultiTypeList<E>();
  }
  
  private List<B> list;
  private final Object lock;

  private MultiTypeList() {

    list = new ArrayList<B>();
    lock = new Object();
  }
  
  public boolean remove(B element) {
    
    if( element == null )
      throw new NullPointerException("element is null.");
    
    synchronized (lock) {

      return list.remove(element);
    }
  }
  
  public void add(B element) {
    
    if( element == null )
      return;
    
    synchronized (lock) {

      list.remove(element);
      list.add(element);
    }
  }

  private List<B> copyList() {

    synchronized (lock) {

      List<B> result = new ArrayList<B>(list);
      return result;
    }
  }
  
  public <E extends B> List<E> getList(Class<E> type) {
    
    if( type == null )
      throw new NullPointerException("type is null.");

    List<B> base = copyList();
    List<E> result = new ArrayList<E>(base.size());

    for(B element : base) {

      if( type.isInstance(element) )
        result.add(type.cast(element));
    }
    return result;
  }
  
  public void clearList(Class<? extends B> type) {
    
    if( type == null )
      throw new NullPointerException("type is null.");
    
    synchronized (lock) {

      List<B> copy = new ArrayList<B>(list.size());
      for(B element : list) {

        if( !type.isInstance(element) )
          copy.add(element);
      }
      list = copy;
    }
  }
  
  public List<? extends B> getAll() {
    
    return copyList();
  }
}
