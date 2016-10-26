/*
 * Copyright 2016 Lei Chen (raistlic@gmail.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.raistlic.common.assertion;

/**
 * Defines a collection of useful checks around a wrapped {@link Number} candidate.
 *
 * @param <N> the actual type of the wrapped candidate.
 */
public interface NumberAssertion<N extends Number & Comparable<N>> extends Assertion<N, NumberAssertion<N>> {

  NumberAssertion<N> greaterThan(N target);

  NumberAssertion<N> greaterThan(N target, String message);

  NumberAssertion<N> greaterThanOrEqualTo(N target);

  NumberAssertion<N> greaterThanOrEqualTo(N target, String message);

  NumberAssertion<N> lessThan(N target);

  NumberAssertion<N> lessThan(N target, String message);

  NumberAssertion<N> lessThanOrEqualTo(N target);

  NumberAssertion<N> lessThanOrEqualTo(N target, String message);
}
