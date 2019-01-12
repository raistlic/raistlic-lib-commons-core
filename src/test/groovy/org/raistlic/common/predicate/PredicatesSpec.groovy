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

import java.util.function.Predicate


class PredicatesSpec extends Specification {

    void 'instanceOf(type) validates parameters and throws exception when type is null'() {
        when:
        Predicates.instanceOf(null)

        then:
        thrown InvalidParameterException
    }

    void 'instanceOf(type) returns expected instance'() {
        given:
        Predicate<Object> predicate = Predicates.instanceOf(Number.class)

        expect:
        predicate.test(123)
        predicate.test(-123)
        predicate.test(123L)
        predicate.test(123.0F)
        predicate.test(123.0)
        predicate.test(new BigDecimal(1234))
        !predicate.test(null)
        !predicate.test(new Object())
        !predicate.test('123')
    }

    void 'builder.not() creates expected instance'() {
        given:
        Predicate<Object> mockBase = Mock(Predicate)
        Predicate<Object> predicate = Predicates.builder(mockBase).not().get()

        and:
        Object candidate = new Object()

        when:
        boolean result = predicate.test(candidate)

        then:
        1 * mockBase.test(candidate) >> false

        and:
        result
    }

    void 'build.or(predicate) creates expected instance'() {
        given:
        Predicate<Object> mockBase = Mock(Predicate)
        Predicate<Object> mockPredicate = Mock(Predicate)
        Predicate<Object> predicate = Predicates.builder(mockBase).or(mockPredicate).get()

        and:
        Object candidate = new Object()

        when:
        boolean result = predicate.test(candidate)

        then:
        1 * mockBase.test(candidate) >> false
        1 * mockPredicate.test(candidate) >> true

        and:
        result
    }

    void 'build.or(predicate) validates parameters and throws exception when predicate is null'() {
        given:
        Predicate<Object> mockBase = Mock(Predicate)

        when:
        Predicates.builder(mockBase).or(null)

        then:
        thrown InvalidParameterException
    }

    void 'build.and(predicate) creates expected instance'() {
        given:
        Predicate<Object> mockBase = Mock(Predicate)
        Predicate<Object> mockPredicate = Mock(Predicate)
        Predicate<Object> predicate = Predicates.builder(mockBase).and(mockPredicate).get()

        and:
        Object candidate = new Object()

        when:
        boolean result = predicate.test(candidate)

        then:
        1 * mockBase.test(candidate) >> true
        1 * mockPredicate.test(candidate) >> false

        and:
        !result
    }

    void 'build.and(predicate) validates parameters and throws exception when predicate is null'() {
        given:
        Predicate<Object> mockBase = Mock(Predicate)

        when:
        Predicates.builder(mockBase).and(null)

        then:
        thrown InvalidParameterException
    }

    void 'builder(base) validates parameters and throws exception when base is null'() {
        when:
        Predicates.builder(null)

        then:
        thrown InvalidParameterException
    }
}