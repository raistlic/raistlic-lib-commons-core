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
package org.raistlic.common.permutation;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

/**
 * This class is to fulfill the needs of getting combinations from a 
 * collection.
 * 
 * It basically provides the functionalities of :
 *   1 - enquiry the number of combination count of combination(m, n)
 *   2 - given an ordinal number i, fetch the i-th combination sequence
 *       as a read-only list.
 *   3 - convenient for-each iteration of all the combinations
 * 
 * This class is NOT thread safe.
 * 
 * This class re-uses one array to fetch the combination, so if the user 
 * want to keep the i-th combination result, make a copy.
 *
 * @author Lei.C
 */
public class Combination<E> implements Iterable<List<E>> {
  
  public static interface Algorithm {

    public int getMaxSupportedSize();

    public BigInteger getCombinationCount(int numberOfElements, int numberToPick);

    public void fetchCombination(Object[] source, Object[] target, BigInteger ordinal);
  }
  
  static final Algorithm DEFAULT_ALGORITHM = DefaultAlgorithm.INSTANCE;
  
  public static <E> Combination<E> of(Collection<E> elements,
                                      int numberToPick) {
    
    return of(elements, numberToPick, DEFAULT_ALGORITHM);
  }
  
  public static <E> Combination<E> of(Collection<E> elements, 
                                      int numberToPick,
                                      Algorithm algorithm) {
    
    if( elements == null )
      throw new NullPointerException("elements collection is null.");
    
    // defensive copy before checking collection size and numberToPick:
    @SuppressWarnings("unchecked")
    E[] arr = (E[])elements.toArray();
    
    if( numberToPick < 0 )
      throw new IllegalArgumentException(
              "Invalid number of elements to pick: " + numberToPick);
    
    if( numberToPick > arr.length )
      throw new IllegalArgumentException(
              "Invalid number of elements to pick: " + 
              numberToPick + " / " + arr.length);
    
    if( algorithm == null )
      algorithm = DEFAULT_ALGORITHM;
    
    return new Combination<E>(arr, numberToPick, algorithm);
  }
  
  /*
   * An internal copy of the elements collection to pick combinations from
   */
  private E[] elements;
  
  /*
   * A buffer that is reused every time to store the picked combination result
   */
  private E[] result;
  
  /*
   * The algorithm to do the combination pick
   */
  private Algorithm algorithm;
  
  /*
   * A cache that stores the number of combinations 
   * C(elements.length, result.length), to avoid frequent repeat calculations.
   */
  private BigInteger count;
  
  @SuppressWarnings("unchecked")
  private Combination(E[] elements, int numberToPick, Algorithm algorithm) {
    
    // no defensive copy here because it's already done in the factory method.
    this.elements = elements;
    this.result = (E[])new Object[numberToPick];
    this.algorithm = algorithm;
    this.count = algorithm.getCombinationCount(elements.length, numberToPick);
  }
  
  public BigInteger getCombinationCount() {
    
    return count;
  }
  
  public List<E> getCombination(BigInteger ordinal) {
    
    if( ordinal == null )
      throw new NullPointerException("ordinal number is null.");
    
    if( ordinal.compareTo(BigInteger.ZERO) < 0 )
      throw new IndexOutOfBoundsException(
              "ordinal number out of range: " + ordinal);
    
    if( ordinal.compareTo(getCombinationCount()) >= 0 )
      throw new IndexOutOfBoundsException(
              "ordinal number out of range: " + ordinal + " / " + getCombinationCount());
    
    algorithm.fetchCombination(elements, result, ordinal);
    return Arrays.<E>asList(result);
  }
  
  /**
   * {@inheritDoc}
   * 
   * <p/>
   * The iterator returned here is a read-only iterator, and does not support 
   * {@link java.util.Iterator#remove()} operation, calling the method will 
   * cause an {@link java.lang.UnsupportedOperationException}.
   */
  @Override
  public Iterator<List<E>> iterator() {
    
    return this.new OrdinalIterator();
  }
  
  private class OrdinalIterator implements Iterator<List<E>> {
    
    private BigInteger ordinal;
    
    private OrdinalIterator() {
      
      ordinal = BigInteger.ZERO;
    }
    
    @Override
    public boolean hasNext() {
      
      return ordinal.compareTo(getCombinationCount()) < 0;
    }

    @Override
    public List<E> next() {
      
      List<E> result = getCombination(ordinal);
      ordinal = ordinal.add(BigInteger.ONE);
      return result;
    }

    @Override
    public void remove() {
      
      throw new UnsupportedOperationException(
              "Cannot remove from a read-only iterator.");
    }
  }
  
  private static enum DefaultAlgorithm implements Algorithm {

    INSTANCE;
    
    @Override
    public int getMaxSupportedSize() {

      return MAX_SUPPORT;
    }

    @Override
    public BigInteger getCombinationCount(int numberOfElements, int numberToPick) {
      
      // no parameter validation because this is only called in the package.
      
      if( numberToPick == 0 || numberToPick == numberOfElements) {
        
        return BigInteger.ONE;
      }
      else {
        
        BigInteger pAll = Factorial.of(numberOfElements);
        BigInteger pPick = Factorial.of(numberToPick);
        BigInteger pRest = Factorial.of(numberOfElements - numberToPick);
        return pAll.divide(pPick.multiply(pRest));
      }
    }

    @Override
    public void fetchCombination(Object[] source, 
                                 Object[] target,
                                 BigInteger ordinal) {
      
      // no parameter validation because this is only called in the package.
      
      for(int i=0, si=0; i<target.length; i++, si++) {

        if( ordinal.compareTo(BigInteger.ZERO) > 0 ) {

          BigInteger cLeft = getCombinationCount(
                  source.length - si - 1, target.length - i - 1);
          while( ordinal.compareTo(cLeft) >= 0 ) {

            si++;
            ordinal = ordinal.subtract(cLeft);
            if( ordinal.compareTo(BigInteger.ZERO) == 0 )
              break;
            cLeft = getCombinationCount(
                    source.length - si - 1, target.length - i - 1);
          }
        }
        target[i] = source[si];
      }
    }
    
    private static final int MAX_SUPPORT = 1024;
  }
}
