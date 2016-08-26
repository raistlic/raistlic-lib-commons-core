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

enum ThreadExpectationPassAll implements ThreadExpectation {

  INSTANCE;

  @Override
  public ThreadExpectation hasId(long id) {

    return this;
  }

  @Override
  public ThreadExpectation hasId(long id, String message) {

    return this;
  }

  @Override
  public ThreadExpectation hasPriority(int priority) {

    return this;
  }

  @Override
  public ThreadExpectation hasPriority(int priority, String message) {

    return this;
  }

  @Override
  public ThreadExpectation isDaemon() {

    return this;
  }

  @Override
  public ThreadExpectation isDaemon(String message) {

    return this;
  }

  @Override
  public ThreadExpectation isNotDaemon() {

    return this;
  }

  @Override
  public ThreadExpectation isNotDaemon(String message) {

    return this;
  }

  @Override
  public ThreadExpectation isInterrupted() {

    return this;
  }

  @Override
  public ThreadExpectation isInterrupted(String message) {

    return this;
  }

  @Override
  public ThreadExpectation isNotInterrupted() {

    return this;
  }

  @Override
  public ThreadExpectation isNotInterrupted(String message) {

    return this;
  }

  @Override
  public ThreadExpectation matches(Predicate<? super Thread> predicate) {

    return this;
  }

  @Override
  public ThreadExpectation matches(Predicate<? super Thread> predicate, String message) {

    return this;
  }
}
