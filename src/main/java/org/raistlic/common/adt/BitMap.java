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

import java.util.Arrays;
import java.util.List;

import org.raistlic.common.Factory;
import org.raistlic.common.condition.Condition;

/**
 * This class implements the "binary rank and select" algorithm.
 * <p/>
 * <p/>
 * The instance of the class is guaranteed to be immutable.
 * <p/>
 * <p/>
 * That is, given a sequence of binary values (1/0), this class helps
 * to quickly perform "rank" and "select" operations.
 * <p/>
 * <p/>
 * A rank operation is:   count the number of 1s (or 0s) up to index i
 * A select operation is: get the index of the i-th 1 (or 0)
 * <p/>
 * <p/>
 * The theoretical efficiency of the original algorithm is constant time
 * operations, using multiple levels of cached maps, while this implementation
 * caches only one level map of every 8 bits.
 *
 * @author Lei.C
 */
public abstract class BitMap {

  /*
   * to keep the immutability promise, this class is designed NOT to be inherited
   * from outside this file.
   */
  private BitMap() {}

  /**
   * A static factory method, which is a shortcut comparing with constructing
   * an instance using a {@code BitMap.Builder}.
   * <p/>
   * <p/>
   * The method will iterate throw the given {@code List}, with the given
   * {@code Condition}, and set 1s at where ever the element on the corresponding
   * index matches the condition.
   *
   * @param <E>  the reference type here is just to make sure, at compile time,
   *             that the given {@code Condition} instance is capable of checking
   *             the elements in the given {@code List}.
   * @param list
   * @param c
   * @return
   */
  public static <E> BitMap newInstance(List<E> list, Condition<? super E> c) {

    if (list == null)
      throw new IllegalArgumentException("List is null.");

    if (c == null)
      throw new IllegalArgumentException("Condition is null.");

    Builder builder = builder(list.size());

    for (int i = 0, size = list.size(); i < size; i++)
      if (c.match(list.get(i)))
        builder.set(i);

    return builder.build();
  }

  /**
   * The static factory method exports a {@code Builder} instance, which provides
   * a more flexible way of creating a {@code BitMap} instance.
   * <p/>
   * <p/>
   * A {@code Builder} is needed mainly because the {@code BitMap} instance
   * is immutable itself, and the {@code Builder} provides the missing "setter"
   * methods, before a {@code BitMap} instance is created.
   * <p/>
   * <p/>
   * As you may expect, a {@code Builder} instance is NOT thread safe.
   *
   * @param size
   * @return
   */
  public static Builder builder(int size) {

    if (size < 0)
      throw new IllegalArgumentException("Invalid bit map size: " + size);

    return new Builder(size);
  }

  public static class Builder implements Factory<BitMap> {

    private final byte[] map;

    private final int size;

    private Builder(int size) {

      this.size = size;
      this.map = new byte[size / 8 + 1];
    }

    public Builder clear() {

      for (int i = 0; i < map.length; i++)
        map[i] = 0;

      return this;
    }

    public Builder set(int index) {

      assert index >= 0;
      assert index < size;

      map[index / 8] |= (1 << (index % 8));
      return this;
    }

    public Builder unset(int index) {

      assert index >= 0;
      assert index < size;

      map[index / 8] &= ~(1 << (index % 8));
      return this;
    }

    @Override
    public BitMap build() {

      return new DefaultBitMap(this);
    }

    /**
     * A {@code BitMap.Builder} is always ready to build, after it is created.
     *
     * @return always true.
     */
    @Override
    public boolean isReady() {

      return true;
    }
  }

  public abstract int size();

  public abstract int rankOne(int index);

  public abstract int rankZero(int index);

  public abstract int selectOne(int index);

  public abstract int selectZero(int index);

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

      if (index < 0)
        throw new IndexOutOfBoundsException(
                "index out of bounds: (" + index + " / " + size + ")");

      if (index >= size)
        throw new IndexOutOfBoundsException(
                "index out of bounds: (" + index + " / " + size + ")");

      int offset = index % 8;
      index /= 8;

      return (index > 0 ? rankOne[index - 1] : 0) + MAP_RANK[0xFF & map[index]][offset];
    }

    @Override
    public int rankZero(int index) {

      if (index < 0)
        throw new IndexOutOfBoundsException(
                "index out of bounds: (" + index + " / " + size + ")");

      if (index >= size)
        throw new IndexOutOfBoundsException(
                "index out of bounds: (" + index + " / " + size + ")");

      return index - rankOne(index) + 1;
    }

    @Override
    public int selectOne(int index) {

      if (index < 0)
        throw new IndexOutOfBoundsException(
                "index out of bounds: (" + index + " / " + size + ")");

      int units = binaryRankSearch(index, rankOne, 0, rankOne.length - 1);
      int counted = units > 0 ? rankOne[units - 1] : 0;

      if (units >= map.length)
        return -1;

      return units * 8 + MAP_SELECT_ONE[0xFF & map[units]][index - counted];
    }

    @Override
    public int selectZero(int index) {

      if (index < 0)
        throw new IndexOutOfBoundsException(
                "index out of bounds: (" + index + " / " + size + ")");

      int units = binaryRankSearch(index, rankZero, 0, rankZero.length - 1);
      int counted = units > 0 ? units * 8 - rankOne[units - 1] : 0;

      if (units >= map.length)
        return -1;

      return units * 8 + MAP_SELECT_ZERO[0xFF & map[units]][index - counted];
    }

    private int binaryRankSearch(int count, int[] rank, int left, int right) {

      if (left == right) {

        return left;
      } else if (left + 1 == right) {

        return rank[left] > count ? left : right;
      } else {

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

      if (index < 0)
        throw new IndexOutOfBoundsException(
                "index out of bounds: (" + index + " / " + size + ")");
      if (index >= size)
        throw new IndexOutOfBoundsException(
                "index out of bounds: (" + index + " / " + size + ")");

      return (map[index / 8] & ~(1 << (index % 8))) != 0;
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

      if (o == this)
        return true;
      else if (o instanceof BitMap) {

        BitMap m = (BitMap) o;
        if (m.size() != size())
          return false;

        for (int i = 0; i < size; i++)
          if (m.isOne(i) != isOne(i))
            return false;

        return true;
      } else
        return false;
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
