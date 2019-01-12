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

        where:
        size | goodCandidate  | badCandidate1 | badCandidate2
        0    | []             | ['foo']       | ['foo', 'bar']
        1    | ['foo']        | []            | ['foo', 'bar']
        2    | ['foo', 'bar'] | []            | ['bar']
    }

    void 'contains(element) creates expected instance'() {
        given:
        Predicate<Collection<Integer>> predicate = CollectionPredicates.contains(3)

        expect:
        predicate.test([1, 2, 3])
        predicate.test([3])
        !predicate.test([1, 2])
        !predicate.test([1, 2, 4, 5])
    }
}
