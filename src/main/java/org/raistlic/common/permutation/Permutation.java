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
 * This class is to fulfill the needs of getting permutations from a
 * collection.
 * <p/>
 * It basically provides the functionalities of :
 * 1 - enquiry the number of permutation count : p(m, n)
 * 2 - given an ordinal number i, fetch the i-th permutation result
 * as a read-only list.
 * 3 - convenient for-each iteration of all the permutations
 * <p/>
 * This class is NOT thread safe.
 * <p/>
 * This class re-uses one array to fetch each enquiry, so if the user
 * want to keep the i-th permutation result, make a copy.
 *
 * @author Lei.C
 */
public class Permutation<E> implements Iterable<List<E>> {

  public static interface Algorithm {

    public int getMaxSupportedSize();

    public BigInteger getPermutationCount(int numberOfElements);

    public void fetchPermutation(Object[] elements, BigInteger ordinal);
  }

  public static final Algorithm DEFAULT_ALGORITHM = DefaultAlgorithm.INSTANCE;

  public static <E> Permutation<E> of(Collection<E> elements) {

    return of(elements, elements.size());
  }

  public static <E> Permutation<E> of(Collection<E> elements,
                                      int numberToPick) {

    return of(elements, numberToPick, DEFAULT_ALGORITHM);
  }

  public static <E> Permutation<E> of(Collection<E> elements,
                                      Algorithm pAlgorithm) {

    return of(elements, elements.size(), pAlgorithm);
  }

  public static <E> Permutation<E> of(Collection<E> elements,
                                      int numberToPick,
                                      Algorithm pAlgorithm) {

    return of(elements, numberToPick, pAlgorithm, Combination.DEFAULT_ALGORITHM);
  }

  public static <E> Permutation<E> of(Collection<E> elements,
                                      int numberToPick,
                                      Permutation.Algorithm pAlgorithm,
                                      Combination.Algorithm cAlgorithm) {

    if (elements == null)
      throw new NullPointerException();

    if (pAlgorithm == null)
      throw new NullPointerException();

    if (elements.size() > pAlgorithm.getMaxSupportedSize())
      throw new IllegalArgumentException(
              "Element collection size not supported by the permutation algorithm.");

    return new Permutation<E>(elements, numberToPick, pAlgorithm, cAlgorithm);
  }


  private E[] elements, picked;

  private Algorithm pAlgorithm;

  private Combination.Algorithm cAlgorithm;

  private BigInteger cCount, pCount;

  private BigInteger count;

  private int numberToPick;

  @SuppressWarnings("unchecked")
  private Permutation(Collection<E> elements,
                      int numberToPick,
                      Algorithm pAlgorithm,
                      Combination.Algorithm cAlgorithm) {

    assert elements != null;
    assert pAlgorithm != null;
    assert cAlgorithm != null;
    assert numberToPick >= 0;
    assert numberToPick <= elements.size();

    this.elements = (E[]) elements.toArray();
    this.picked = (E[]) new Object[numberToPick];
    this.pAlgorithm = pAlgorithm;
    this.cAlgorithm = cAlgorithm;
    this.numberToPick = numberToPick;

    this.cCount = this.cAlgorithm.getCombinationCount(this.elements.length,
            this.numberToPick);
    this.pCount = this.pAlgorithm.getPermutationCount(this.numberToPick);
    this.count = this.cCount.multiply(this.pCount);
  }

  public BigInteger getPermutationCount() {

    return count;
  }

  public List<E> getPermutation(BigInteger ordinal) {

    if (ordinal == null)
      throw new NullPointerException();

    if (ordinal.compareTo(BigInteger.ZERO) < 0 || ordinal.compareTo(count) >= 0)
      throw new IllegalArgumentException(
              "Ordinal value out of range : " + ordinal);

    if (numberToPick == elements.length) {

      System.arraycopy(elements, 0, picked, 0, elements.length);
    }
    else {

      cAlgorithm.fetchCombination(elements, picked, ordinal.divide(pCount));
      ordinal = ordinal.mod(pCount);
    }
    pAlgorithm.fetchPermutation(picked, ordinal);
    return Arrays.asList(picked);
  }

  @Override
  public Iterator<List<E>> iterator() {

    return this.new OrdinalIterator();
  }

  private class OrdinalIterator implements Iterator<List<E>> {

    private BigInteger ordinal;

    private OrdinalIterator() {

      this.ordinal = BigInteger.ZERO;
    }

    @Override
    public boolean hasNext() {

      return ordinal.compareTo(count) < 0;
    }

    @Override
    public List<E> next() {

      List<E> result = getPermutation(ordinal); // throws IndexOutOfRangeException
      ordinal = ordinal.add(BigInteger.ONE);
      return result;
    }

    @Override
    public void remove() {

      throw new UnsupportedOperationException();
    }
  }

  private static enum DefaultAlgorithm implements Algorithm {

    INSTANCE;

    @Override
    public int getMaxSupportedSize() {

      return MAX_SUPPORT;
    }

    @Override
    public BigInteger getPermutationCount(int numberOfElements) {

      if (numberOfElements < 0)
        throw new IllegalArgumentException(
                "Invalid number of elements : " + numberOfElements);

      if (numberOfElements > getMaxSupportedSize())
        throw new IllegalArgumentException(
                "Number of elements out of range : " + numberOfElements);

      return Factorial.of(numberOfElements);
    }

    @Override
    public void fetchPermutation(Object[] elements, BigInteger ordinal) {

      if (elements == null)
        throw new NullPointerException("elements array is null.");

      if (ordinal == null)
        throw new NullPointerException("ordinal number is null.");

      if (ordinal.compareTo(BigInteger.ZERO) < 0)
        throw new IllegalArgumentException(
                "ordinal number out of range: " + ordinal);

      if (ordinal.compareTo(getPermutationCount(elements.length)) >= 0)
        throw new IllegalArgumentException(
                "ordinal number out of range: " +
                        ordinal + " / " + getPermutationCount(elements.length));

      for (int i = 0; i < elements.length - 1; i++) {

        int left = elements.length - i - 1;
        BigInteger leftCount = Factorial.of(left);
        int curr = ordinal.divide(leftCount).intValue();
        ordinal = ordinal.mod(leftCount);
        if (curr > 0) {

          Object temp = elements[curr + i];
          for (int j = curr + i; j > i; j--)
            elements[j] = elements[j - 1];
          elements[i] = temp;
        }
      }
    }

    private static final int MAX_SUPPORT = 1024;
  }
}
