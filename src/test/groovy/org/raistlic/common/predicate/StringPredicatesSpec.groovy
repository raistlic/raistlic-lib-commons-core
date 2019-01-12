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
import java.util.regex.Pattern


class StringPredicatesSpec extends Specification {

    void 'isEmpty() returns expected instance'() {
        given:
        Predicate<String> predicate = StringPredicates.isEmpty()

        expect:
        predicate.test('')
        !predicate.test(null)
        !predicate.test('foo')
    }

    void 'notEmpty() returns expected instance'() {
        given:
        Predicate<String> predicate = StringPredicates.notEmpty()

        expect:
        !predicate.test('')
        predicate.test(null)
        predicate.test('foo')
    }

    @Unroll
    void 'hasLength(length) validates parameters and throws exception for invalid length #length'() {
        when:
        StringPredicates.hasLength(length)

        then:
        thrown InvalidParameterException

        where:
        length << [-1, -2, -3, -100, Integer.MIN_VALUE]
    }

    void 'hasLength(length) returns expected instance'() {
        given:
        Predicate<String> predicate = StringPredicates.hasLength(10)

        expect:
        predicate.test('1234567890')
        predicate.test('          ')
        predicate.test('test test ')
        !predicate.test('test test  ')
        !predicate.test('test test')
        !predicate.test('foo')
        !predicate.test('')
        !predicate.test(null)
    }

    void 'matchesPattern(pattern) validates parameters and throws exception when pattern is null'() {
        when:
        StringPredicates.matchesPattern(null)

        then:
        thrown InvalidParameterException
    }

    void 'matchesPattern(pattern) returns expected result'() {
        when:
        Predicate<String> predicate = StringPredicates.matchesPattern(Pattern.compile('^\\d+$'))

        then:
        predicate.test('123')
        !predicate.test('123abc')
        !predicate.test('')
        !predicate.test(null)
        !predicate.test('foo')
    }
}