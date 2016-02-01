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

import org.raistlic.common.precondition.Precondition;
import org.raistlic.common.util.Factory;

import java.util.Arrays;
import java.util.List;
import java.util.function.Predicate;

/**
 * This class implements the "binary rank and select" algorithm.
 * <p>
 * The instance of the class is guaranteed to be immutable.
 * <p>
 * That is, given a sequence of binary values (1/0), this class helps
 * to quickly perform "rank" and "select" operations.
 * <p>
 * A rank operation is:   count the number of 1s (or 0s) up to index i
 * A select operation is: get the index of the i-th 1 (or 0)
 * <p>
 * The theoretical efficiency of the original algorithm is constant time
 * operations, using multiple levels of cached maps, while this implementation
 * caches only one level map of every 8 bits.
 *
 * @author Lei CHEN
 * @since 1.0
 */
public abstract class BitMap {

  /*
   * to keep the immutability promise, this class is designed NOT to be inherited
   * from outside this file.
   */
  private BitMap() { }

  /**
   * A static factory method, which is a shortcut comparing with constructing
   * an instance using a {@code BitMap.Builder}.
   * <p>
   * The method will iterate throw the given {@code List}, with the given
   * {@code Condition}, and set 1s at where ever the element on the corresponding
   * index matches the condition.
   *
   * @param <E>       the reference type here is just to make sure, at compile time,
   *                  that the given {@code Condition} instance is capable of checking
   *                  the elements in the given {@code List}.
   * @param list      the list of elements based on which to create the bit map.
   * @param condition the condition to check the {@code list}
   * @return the created bit map.
   */
  public static <E> BitMap newInstance(List<E> list, Predicate<? super E> condition) {

    Precondition.param(list, "list").notNull();
    Precondition.param(condition, "condition").notNull();

    Builder builder = builder(list.size());

    for (int i = 0, size = list.size(); i < size; i++)
      if (condition.test(list.get(i)))
        builder.set(i);

    return builder.build();
  }

  /**
   * The static factory method exports a {@code Builder} instance, which provides
   * a more flexible way of creating a {@code BitMap} instance.
   * <p>
   * A {@code Builder} is needed mainly because the {@code BitMap} instance
   * is immutable itself, and the {@code Builder} provides the missing "setter"
   * methods, before a {@code BitMap} instance is created.
   * <p>
   * As you may expect, a {@code Builder} instance is NOT thread safe.
   *
   * @param size the size of the bit map to build, cannot be less than {@code 0} .
   * @return the new {@link org.raistlic.common.adt.BitMap.Builder} instance.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException if {@code size} is less than
   *         {@code 0}.
   */
  public static Builder builder(int size) {

    Precondition.param(size, "size").noLessThan(0);

    return new Builder(size);
  }

  /**
   * The builder to create new bit map instances.
   */
  public static class Builder implements Factory<BitMap> {

    private final byte[] map;

    private final int size;

    private Builder(int size) {

      this.size = size;
      this.map = new byte[size / 8 + 1];
    }

    /**
     * Set all bits to be {@code 0} .
     *
     * @return the {@link org.raistlic.common.adt.BitMap.Builder} instance itself.
     */
    public Builder clear() {

      for (int i = 0; i < map.length; i++)
        map[i] = 0;

      return this;
    }

    /**
     * The method sets the bit at {@code index} to be {@code 1} .
     *
     * @param index the index of the bit to set, must be within range {@code [0, size)}.
     * @return the {@link org.raistlic.common.adt.BitMap.Builder} instance itself.
     *
     * @throws org.raistlic.common.precondition.InvalidParameterException if {@code index} is out of
     *         range.
     */
    public Builder set(int index) {

      Precondition.param(index, "index").noLessThan(0);
      Precondition.param(index, "index").lessThan(size);

      map[index / 8] |= (1 << (index % 8));
      return this;
    }

    /**
     * The method sets the bit at {@code index} to be {@code 0} .
     *
     * @param index the index of the bit to clear, must be within range {@code [0, size)}.
     * @return the {@link org.raistlic.common.adt.BitMap.Builder} instance itself.
     *
     * @throws org.raistlic.common.precondition.InvalidParameterException if {@code index} is out of
     *         range.
     */
    public Builder unset(int index) {

      Precondition.param(index, "index").noLessThan(0);
      Precondition.param(index, "index").lessThan(size);

      map[index / 8] &= ~(1 << (index % 8));
      return this;
    }

    /**
     * Create and return a new {@link BitMap} instance based on the current state of the
     * {@code builder} .
     *
     * @return the created {@link BitMap} .
     */
    @Override
    public BitMap build() {

      return new DefaultBitMap(this);
    }
  }

  /**
   * The method returns the size of the {@link BitMap} .
   *
   * @return the size of the {@link BitMap} .
   */
  public abstract int size();

  /**
   * The method returns the number of {@code 1} s up to {@code index} (inclusively).
   *
   * @param index the index up to which to query the number of {@code 1} s, must be within the
   *              range {@code [0, size())} .
   * @return the number of {@code 1} s.
   * @throws org.raistlic.common.precondition.InvalidParameterException if {@code index} is out of
   *                                                                    range.
   */
  public abstract int rankOne(int index);

  /**
   * The method returns the number of {@code 0} s up to the {@code index} (inclusively).
   *
   * @param index the index up to which to query the number of {@code 0} s, must be within the
   *              range {@code [0, size())} .
   * @return the number of {@code 0} s.
   * @throws org.raistlic.common.precondition.InvalidParameterException if {@code index} is out of
   *                                                                    range.
   */
  public abstract int rankZero(int index);

  /**
   * The method returns the index of the {@code i}-th {@code 1} .
   *
   * @param i specifies which {@code 1} 's index to query, cannot be less than {@code 0} .
   * @return the index of the {@code i}-th {@code 1}, or {@code -1} if there are insufficient
   * {@code 1} s in the bit map.
   * @throws org.raistlic.common.precondition.InvalidParameterException if {@code i} is less than
   *                                                                    {@code 0}.
   */
  public abstract int selectOne(int i);

  /**
   * The method returns the index of the {@code i}-th {@code 0} .
   *
   * @param i specifies which {@code 0} 's index to query, cannot be less than {@code 0} .
   * @return the index of the {@code i}-th {@code 0}, or {@code -1} if there are insufficient
   * {@code 1} s in the bit map.
   * @throws org.raistlic.common.precondition.InvalidParameterException if {@code i} is less than
   *                                                                    {@code 0}.
   */
  public abstract int selectZero(int i);

  /**
   * The method returns whether the specified bit at {@code index} is {@code 1} or not.
   *
   * @param index the index to query, must be within the range {@code [0, size())} .
   * @return {@code true} if the bit at {@code index} is {@code 1} .
   * @throws org.raistlic.common.precondition.InvalidParameterException if {@code index} is out of
   *                                                                    range.
   */
  public abstract boolean isOne(int index);

  private static class DefaultBitMap extends BitMap {

    private final byte[] map;

    private final int[] rankOne;

    private final int[] rankZero;

    private final int size;

    private DefaultBitMap(Builder builder) {

      int len = builder.map.length;
      map = Arrays.copyOf(builder.map, len);
      size = builder.size;

      rankOne = new int[len];
      rankZero = new int[len];
      for (int i = 0; i < len; i++) {

        rankOne[i] = MAP_RANK[0xFF & map[i]][7];
        if (i > 0)
          rankOne[i] += rankOne[i - 1];
        rankZero[i] = (i + 1) * 8 - rankOne[i];
      }
    }

    @Override
    public int size() {

      return size;
    }

    @Override
    public int rankOne(int index) {

      Precondition.param(index, "index").noLessThan(0);
      Precondition.param(index, "index").lessThan(size);

      int offset = index % 8;
      index /= 8;

      return (index > 0 ? rankOne[index - 1] : 0) + MAP_RANK[0xFF & map[index]][offset];
    }

    @Override
    public int rankZero(int index) {

      Precondition.param(index, "index").noLessThan(0);
      Precondition.param(index, "index").lessThan(size);

      return index - rankOne(index) + 1;
    }

    @Override
    public int selectOne(int i) {

      Precondition.param(i, "i").noLessThan(0);

      if (i >= rankOne(size - 1)) {
        return -1;
      }

      int units = binaryRankSearch(i, rankOne, 0, rankOne.length - 1);
      int counted = units > 0 ? rankOne[units - 1] : 0;

      return units * 8 + MAP_SELECT_ONE[0xFF & map[units]][i - counted];
    }

    @Override
    public int selectZero(int i) {

      Precondition.param(i, "i").noLessThan(0);

      if (i >= rankZero(size - 1)) {
        return -1;
      }

      int units = binaryRankSearch(i, rankZero, 0, rankZero.length - 1);
      int counted = units > 0 ? units * 8 - rankOne[units - 1] : 0;

      return units * 8 + MAP_SELECT_ZERO[0xFF & map[units]][i - counted];
    }

    private int binaryRankSearch(int count, int[] rank, int left, int right) {

      if (left == right) {

        return left;
      }
      else if (left + 1 == right) {

        return rank[left] > count ? left : right;
      }
      else {

        int mid = (left + right) / 2;
        if (rank[mid] > count)
          right = mid;
        else
          left = mid;
        return binaryRankSearch(count, rank, left, right);
      }
    }

    @Override
    public boolean isOne(int index) {

      Precondition.param(index, "index").noLessThan(0);
      Precondition.param(index, "index").lessThan(size);

      return (map[index / 8] & (1 << (index % 8))) != 0;
    }

    @Override
    public int hashCode() {

      int result = size();

      int[] tokens = new int[result / 16 + 1];
      for (int i = 0; i < result; i++)
        if (isOne(i))
          tokens[i / 16] |= 1 << (i % 16);

      for (int token : tokens)
        result = (result << 5) + token - result;
      return result;
    }

    @Override
    public boolean equals(Object o) {

      if (o == this) {

        return true;
      }
      else if (o instanceof BitMap) {

        BitMap m = (BitMap) o;
        if (m.size() != size()) {
          return false;
        }
        for (int i = 0; i < size; i++) {
          if (m.isOne(i) != isOne(i)) {
            return false;
          }
        }
        return true;
      }
      else {

        return false;
      }
    }
  }

  private static final int[][] MAP_RANK;

  private static final int[][] MAP_SELECT_ONE;

  private static final int[][] MAP_SELECT_ZERO;

  static {

    MAP_RANK = new int[256][8]; // 256 x 8 x 4 =~ 8KB
    for (int i = 0; i < 256; i++) {

      for (int j = 0; j < 8; j++)
        MAP_RANK[i][j] = countOne(i, j + 1);
    }

    MAP_SELECT_ONE = new int[256][8]; // 256 x 8 x 4 =~ 8KB
    for (int i = 0; i < 256; i++) {

      for (int j = 1, offset = 0; j < 8; j++) {

        while (offset < 8 && MAP_RANK[i][offset] < j)
          offset++;

        MAP_SELECT_ONE[i][j - 1] = offset >= 8 ? -1 : offset;
      }
    }

    MAP_SELECT_ZERO = new int[256][8]; // 256 x 8 x 4 =~ 8KB
    for (int i = 0; i < 256; i++) {

      for (int j = 1, offset = 0; j < 8; j++) {

        while (offset < 8 && countZero(i, offset + 1) < j)
          offset++;

        MAP_SELECT_ZERO[i][j - 1] = offset >= 8 ? -1 : offset;
      }
    }
  }

  private static int countOne(int pattern, int bits) {

    int count = 0;
    for (int i = 0; i < bits; i++)
      if (((1 << i) & pattern) != 0)
        count++;
    return count;
  }

  private static int countZero(int pattern, int bits) {

    int count = 0;
    for (int i = 0; i < bits; i++)
      if (((1 << i) & pattern) == 0)
        count++;
    return count;
  }
}
