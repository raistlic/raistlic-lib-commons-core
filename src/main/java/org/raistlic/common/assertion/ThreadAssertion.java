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

import java.util.function.Predicate;

/**
 * Defines a collection of useful checks around a wrapped {@link Thread} candidate.
 */
public interface ThreadAssertion {

  ThreadAssertion hasId(long id);

  ThreadAssertion hasId(long id, String message);

  ThreadAssertion hasPriority(int priority);

  ThreadAssertion hasPriority(int priority, String message);

  ThreadAssertion isDaemon();

  ThreadAssertion isDaemon(String message);

  ThreadAssertion isNotDaemon();

  ThreadAssertion isNotDaemon(String message);

  ThreadAssertion isInterrupted();

  ThreadAssertion isInterrupted(String message);

  ThreadAssertion isNotInterrupted();

  ThreadAssertion isNotInterrupted(String message);

  ThreadAssertion matches(Predicate<? super Thread> predicate);

  ThreadAssertion matches(Predicate<? super Thread> predicate, String message);
}
