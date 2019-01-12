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

import spock.lang.Specification
import spock.lang.Unroll


class DummyPredicateSpec extends Specification {

    @Unroll
    void 'DummyPredicate.TRUE returns true for #senario'() {
        expect:
        DummyPredicate.TRUE.test(candidate)

        where:
        senario         | candidate
        'random object' | new Object()
        'number'        | 123
        'string'        | 'string'
        'empty string'  | ''
        'null'          | null
    }

    @Unroll
    void 'DummyPredicate.FALSE returns false for #senario'() {
        expect:
        !DummyPredicate.FALSE.test(candidate)

        where:
        senario         | candidate
        'random object' | new Object()
        'number'        | 123
        'string'        | 'string'
        'empty string'  | ''
        'null'          | null
    }
}