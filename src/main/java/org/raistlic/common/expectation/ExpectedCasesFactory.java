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

import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Function;

public final class ExpectedCasesFactory {

  private Function<String, ? extends RuntimeException> exceptionMapper;

  private ExpectedCases.Strategy strategy;

  private final AtomicBoolean switchFlag;

  public ExpectedCasesFactory(Function<String, ? extends RuntimeException> exceptionMapper, ExpectedCases.Strategy strategy) {

    this.exceptionMapper = exceptionMapper;
    this.strategy = strategy;
    this.switchFlag = new AtomicBoolean(false);
  }

  public void setSwitch(boolean flag) {

    switchFlag.set(flag);
  }

  public Optional<ExpectedCases> setExceptionMapper(Function<String, ? extends RuntimeException> exceptionMapper) {

    synchronized (this) {
      if (exceptionMapper != this.exceptionMapper) {
        this.exceptionMapper = exceptionMapper;
        return Optional.of(create());
      }
      else {
        return Optional.empty();
      }
    }
  }

  public Optional<ExpectedCases> setExpectedCasesStrategy(ExpectedCases.Strategy strategy) {

    synchronized (this) {
      if (strategy != this.strategy) {
        this.strategy = strategy;
        return Optional.of(create());
      }
      else {
        return Optional.empty();
      }
    }
  }

  public ExpectedCases create() {

    synchronized (this) {
      ExpectedCases expectedCases = (strategy == ExpectedCases.Strategy.CREATE_NEW) ?
          Expectations.createDefaultExpectedCases(exceptionMapper) : Expectations.createThreadLocalExpectedCases(exceptionMapper);
      return Expectations.createSwitchableProxy(expectedCases, switchFlag);
    }
  }
}
