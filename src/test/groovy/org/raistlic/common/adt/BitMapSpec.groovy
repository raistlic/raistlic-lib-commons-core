package org.raistlic.common.adt

import org.raistlic.common.precondition.InvalidParameterException
import org.raistlic.common.predicate.Predicates
import spock.lang.Shared
import spock.lang.Specification

class BitMapSpec extends Specification {

  @Shared
  BitMap bitMap = BitMap.builder(10)
      .set(1)
      .set(3)
      .set(4)
      .set(7)
      .build();

  def "newInstance with null list should throw exception"() {
    when:
    BitMap.newInstance(null, Predicates.dummyTrue())
    then:
    thrown(InvalidParameterException)
  }

  def "newInstance with null condition should throw exception"() {
    when:
    BitMap.newInstance([1, 2, 3], null)
    then:
    thrown(InvalidParameterException)
  }

  def "newInstance should create non null BitMap when given valid parameters"() {
    expect:
    BitMap.newInstance(list, condition) != null
    where:
    list            | condition
    [1, 2, 3]       | Predicates.dummyTrue()
    ["a", "b", "c"] | Predicates.dummyFalse()
    []              | Predicates.isNull()
  }

  def "builder with negative size should throw exception"() {
    when:
    BitMap.builder(size)
    then:
    thrown(InvalidParameterException)
    where:
    size << [-1, -2, -3, -100, Integer.MIN_VALUE]
  }

  def "builder(#size) should return a non null BitMap.Builder instance"() {
    expect:
    BitMap.builder(size) != null
    where:
    size << [0, 1, 2, 3, 4, 99, 100, 9999, Integer.MAX_VALUE]
  }

  def "size() should return #size"() {
    expect:
    BitMap.newInstance(list, condition).size() == size
    where:
    list                      | condition               | size
    []                        | Predicates.dummyTrue()  | 0
    []                        | Predicates.isNull()     | 0
    []                        | Predicates.dummyFalse() | 0
    ["a"]                     | Predicates.dummyFalse() | 1
    [1, 2, 3, 4, 5]           | Predicates.dummyTrue()  | 5
    createListWithSize(99)    | Predicates.dummyTrue()  | 99
    createListWithSize(12345) | Predicates.dummyFalse() | 12345
  }

  def "rankOne(#index) should return #rank"() {
    // 0 1 0 1 1 0 0 1 0 0
    // 0 1 2 3 4 5 6 7 8 9
    expect:
    bitMap.rankOne(index) == rank
    where:
    index | rank
    0     | 0
    1     | 1
    2     | 1
    3     | 2
    4     | 3
    5     | 3
    6     | 3
    7     | 4
    8     | 4
    9     | 4
  }

  def "rankZero(#index) should return #rank"() {
    // 0 1 0 1 1 0 0 1 0 0
    // 0 1 2 3 4 5 6 7 8 9
    expect:
    bitMap.rankZero(index) == rank
    where:
    index | rank
    0     | 1
    1     | 1
    2     | 2
    3     | 2
    4     | 2
    5     | 3
    6     | 4
    7     | 4
    8     | 5
    9     | 6
  }

  def "selectOne(#rank) should return #index"() {
    // 0 1 0 1 1 0 0 1 0 0
    // 0 1 2 3 4 5 6 7 8 9
    expect:
    bitMap.selectOne(rank) == index
    where:
    rank | index
    0    | 1
    1    | 3
    2    | 4
    3    | 7
    4    | -1
    5    | -1
    6    | -1
    7    | -1
    8    | -1
    9    | -1
  }
  
  def "selectZero(#rank) should return #index"() {
    // 0 1 0 1 1 0 0 1 0 0
    // 0 1 2 3 4 5 6 7 8 9
    expect:
    bitMap.selectZero(rank) == index
    where:
    rank | index
    0    | 0
    1    | 2
    2    | 5
    3    | 6
    4    | 8
    5    | 9
    6    | -1
    7    | -1
    8    | -1
    9    | -1
  }

  def createListWithSize(int size) {
    List<Object> result = []
    while (size > 0) {
      size--
      result.add(new Object())
    }
    return result
  }
}
