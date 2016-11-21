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

package org.raistlic.common.permutation;

import org.raistlic.common.precondition.Precondition;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 * Utility class for easy query of all possible combinations of "picking m elements from an n-size 
 * collection".
 * 
 * It basically provides the functionalities of :
 *   1 - enquiry the number of combination count of combination(m, n)
 *   2 - given an ordinal number i, fetch the i-th combination sequence as a read-only list.
 *   3 - convenient for-each iteration of all the combinations
 * 
 * It is NOT thread safe.
 * 
 * It re-uses one array to fetch the combination, so if the user want to keep the i-th combination 
 * result, make a copy.
 */
public class Combination<E> implements Iterable<List<E>> {

  /**
   * A callback interface that implements the combination algorithm.
   */
  public interface Algorithm {

    /**
     * Returns the maximum size of collection that the algorithm implementation supports, it is 
     * {@link Integer#MAX_VALUE} by default.
     * 
     * @return the maximum size of collection that the algorithm implementation supports.
     */
    default int getMaxSupportedSize() {
      return Integer.MAX_VALUE;
    }

    /**
     * Returns the number of all possible combinations, for picking {@code numberToPick} elements 
     * from a collection of size {@code numberOfElements}.
     * 
     * @param numberOfElements the collection size, must be no less than {@code 0} .
     * @param numberToPick the number of elements to pick, must be no less than {@code 0} and no more
     *                     than {@code numberOfElements} .
     * @return the number of all possible combinations.
     * 
     * @throws org.raistlic.common.precondition.InvalidParameterException when {@code numberOfElements} 
     *         or {@code numberToPick} is invalid.
     */
    BigInteger getCombinationCount(int numberOfElements, int numberToPick);

    /**
     * Fills the {@code target} array with selected elements in the {@code source} array, as the 
     * result of the {@code ordinal}-th combination result. 
     * 
     * @param source the source array, cannot be {@code null}
     * @param target the target array, cannot be {@code null}, and it's size cannot be bigger than 
     *               {@code source} .
     * @param ordinal the index of the combination result to pick, must be no less than {@code 0} and 
     *                must be less than {@code getCombinationCount(source.length, target.length)} .
     *                
     * @throws org.raistlic.common.precondition.InvalidParameterException when any of the parameters
     *         is invalid.
     */
    void fetchCombination(Object[] source, Object[] target, BigInteger ordinal);
  }
  
  static final Algorithm DEFAULT_ALGORITHM = DefaultAlgorithm.INSTANCE;

  /**
   * Factory method that exports a combination instance with the specified {@code elements} collection
   * and {@code numberToPick} , using the default algorithm implementation.
   * 
   * @param elements the elements collection to pick combination from, cannot be {@code null}.
   * @param numberToPick the number of elements to pick, must be no less than {@code 0} and no more
   *                     than {@code elements} size.
   * @param <E> the element type.
   * @return the combination instance.
   * 
   * @throws org.raistlic.common.precondition.InvalidParameterException when any of the parameters
   *         is invalid.
   */
  public static <E> Combination<E> of(Collection<E> elements, int numberToPick) {
    
    return of(elements, numberToPick, DEFAULT_ALGORITHM);
  }

  /**
   * Factory method that exports a combination instance with the specified {@code elements} collection
   * and {@code numberToPick} , using the {@code algorithm} callback provided, or the default algorithm
   * if the {@code algorithm} parameter passed in is {@code null}.
   * 
   * @param elements the elements collection to pick combination from, cannot be {@code null}.
   * @param numberToPick the number of elements to pick, must be no less than {@code 0} and no more
   *                     than {@code elements} size.
   * @param algorithm the algorithm as a callback for picking combinations, or {@code null} if using 
   *                  the default algorithm.
   * @param <E> the element type.
   * @return the combination instance.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when any of the parameters
   *         is invalid.
   */
  public static <E> Combination<E> of(Collection<E> elements, int numberToPick, Algorithm algorithm) {

    if (algorithm == null) {
      algorithm = DEFAULT_ALGORITHM;
    }
    
    Precondition.param(elements).isNotNull("'elements' cannot be null.");
    Precondition.param(elements.size()).lessThanOrEqualTo(
        algorithm.getMaxSupportedSize(), 
        "'elements' size is too big and not supported by the algorithm: " + elements.size() 
            + " / " + algorithm.getMaxSupportedSize()
    );
    Precondition.param(numberToPick)
        .greaterThanOrEqualTo(0, "Invalid 'numberToPick': " + numberToPick)
        .lessThanOrEqualTo(elements.size(), "Invalid 'numberToPick': " + numberToPick);
    
    // defensive copy before checking collection size and numberToPick:
    @SuppressWarnings("unchecked")
    E[] arr = (E[])elements.toArray();
    return new Combination<>(arr, numberToPick, algorithm);
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
  
  /*
   * Wraps result as an unmodifiable list.
   */
  private List<E> resultList;
  
  @SuppressWarnings("unchecked")
  private Combination(E[] elements, int numberToPick, Algorithm algorithm) {
    
    // no defensive copy here because it's already done in the factory method.
    this.elements = elements;
    this.result = (E[])new Object[numberToPick];
    this.algorithm = algorithm;
    this.count = algorithm.getCombinationCount(elements.length, numberToPick);
    this.resultList = Collections.unmodifiableList(Arrays.asList(result));
  }

  /**
   * Returns the number of all possible combinations.
   * 
   * @return the number of all possible combinations.
   */
  public BigInteger getCombinationCount() {
    
    return count;
  }

  /**
   * Returns the {@code ordinal}-th combination result, as a {@link List} .
   * 
   * @param ordinal the index of the combination result to return, cannot be {@code null} and must 
   *                be no less than {@code 0} and must less than {@link #getCombinationCount()}. 
   * @return the {@code ordinal}-th combination result.
   * 
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code ordinal} is 
   *         {@code null} or out of the valid range.
   */
  public List<E> getCombination(BigInteger ordinal) {

    Precondition.param(ordinal)
        .isNotNull("ordinal number cannot be null.")
        .greaterThanOrEqualTo(BigInteger.ZERO, "ordinal number out of range: " + ordinal)
        .lessThan(getCombinationCount(), "ordinal number out of range: " + ordinal + " / " + getCombinationCount());
    
    algorithm.fetchCombination(elements, result, ordinal);
    return resultList;
  }
  
  /**
   * {@inheritDoc}
   * 
   * <p>
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
  
  private enum DefaultAlgorithm implements Algorithm {

    INSTANCE;
    
    @Override
    public int getMaxSupportedSize() {

      return Integer.MAX_VALUE;
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
    public void fetchCombination(Object[] source, Object[] target, BigInteger ordinal) {
      
      // no parameter validation because this is only called in the package.
      
      for(int i=0, si=0; i<target.length; i++, si++) {

        if( ordinal.compareTo(BigInteger.ZERO) > 0 ) {

          BigInteger cLeft = getCombinationCount(source.length - si - 1, target.length - i - 1);
          while( ordinal.compareTo(cLeft) >= 0 ) {
            si++;
            ordinal = ordinal.subtract(cLeft);
            if( ordinal.compareTo(BigInteger.ZERO) == 0 ) {
              break;
            }
            cLeft = getCombinationCount(source.length - si - 1, target.length - i - 1);
          }
        }
        target[i] = source[si];
      }
    }
  }
}
