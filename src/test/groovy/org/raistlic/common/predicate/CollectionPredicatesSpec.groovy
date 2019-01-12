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
package org.raistlic.common.predicate

import org.raistlic.common.precondition.InvalidParameterException
import spock.lang.Specification
import spock.lang.Unroll

import java.util.function.Predicate

class CollectionPredicatesSpec extends Specification {

    void 'isEmpty() creates expected instance'() {
        given:
        Predicate<Collection> predicate = CollectionPredicates.isEmpty()

        expect:
        predicate != null
        predicate.test([])
        !predicate.test(null)
        !predicate.test(['foo'])
    }

    void 'notEmpty() creates expected instance'() {
        given:
        Predicate<Collection> predicate = CollectionPredicates.notEmpty()

        expect:
        predicate != null
        !predicate.test([])
        predicate.test(null)
        predicate.test(['foo'])
    }

    @Unroll
    void 'hasSize(size) validates parameters and throws exception when size is #size'() {
        when:
        CollectionPredicates.hasSize(size)

        then:
        thrown InvalidParameterException

        where:
        size << [-1, -2, -100, Integer.MIN_VALUE]
    }

    @Unroll
    void 'hasSize(size) creates expected instance with #size'() {
        given:
        Predicate<Collection> predicate = CollectionPredicates.hasSize(size)

        expect:
        predicate != null
        predicate.test(goodCandidate)
        !predicate.test(badCandidate1)
        !predicate.test(badCandidate2)
        !predicate.test(badCandidate3)

        where:
        size | goodCandidate  | badCandidate1 | badCandidate2  | badCandidate3
        0    | []             | ['foo']       | ['foo', 'bar'] | null
        1    | ['foo']        | []            | ['foo', 'bar'] | null
        2    | ['foo', 'bar'] | []            | ['bar']        | null
    }

    void 'contains(element) creates expected instance'() {
        given:
        Predicate<Collection<Integer>> predicate = CollectionPredicates.contains(3)

        expect:
        predicate.test([1, 2, 3])
        predicate.test([3])
        !predicate.test([1, 2])
        !predicate.test([1, 2, 4, 5])
        !predicate.test(null)
    }
}
