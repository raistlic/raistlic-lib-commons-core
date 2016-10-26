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

enum ThreadAssertionPassAll implements ThreadAssertion {

  INSTANCE;

  @Override
  public ThreadAssertion hasId(long id) {

    return this;
  }

  @Override
  public ThreadAssertion hasId(long id, String message) {

    return this;
  }

  @Override
  public ThreadAssertion hasPriority(int priority) {

    return this;
  }

  @Override
  public ThreadAssertion hasPriority(int priority, String message) {

    return this;
  }

  @Override
  public ThreadAssertion isDaemon() {

    return this;
  }

  @Override
  public ThreadAssertion isDaemon(String message) {

    return this;
  }

  @Override
  public ThreadAssertion isNotDaemon() {

    return this;
  }

  @Override
  public ThreadAssertion isNotDaemon(String message) {

    return this;
  }

  @Override
  public ThreadAssertion isInterrupted() {

    return this;
  }

  @Override
  public ThreadAssertion isInterrupted(String message) {

    return this;
  }

  @Override
  public ThreadAssertion isNotInterrupted() {

    return this;
  }

  @Override
  public ThreadAssertion isNotInterrupted(String message) {

    return this;
  }

  @Override
  public ThreadAssertion matches(Predicate<? super Thread> predicate) {

    return this;
  }

  @Override
  public ThreadAssertion matches(Predicate<? super Thread> predicate, String message) {

    return this;
  }
}
