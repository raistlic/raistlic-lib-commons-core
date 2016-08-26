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

package org.raistlic.common.expectation;

import java.util.function.Predicate;

/**
 * Defines a collection of useful checks around a wrapped {@link Thread} candidate.
 */
public interface ThreadExpectation {

  ThreadExpectation hasId(long id);

  ThreadExpectation hasId(long id, String message);

  ThreadExpectation hasPriority(int priority);

  ThreadExpectation hasPriority(int priority, String message);

  ThreadExpectation isDaemon();

  ThreadExpectation isDaemon(String message);

  ThreadExpectation isNotDaemon();

  ThreadExpectation isNotDaemon(String message);

  ThreadExpectation isInterrupted();

  ThreadExpectation isInterrupted(String message);

  ThreadExpectation isNotInterrupted();

  ThreadExpectation isNotInterrupted(String message);

  ThreadExpectation matches(Predicate<? super Thread> predicate);

  ThreadExpectation matches(Predicate<? super Thread> predicate, String message);
}
