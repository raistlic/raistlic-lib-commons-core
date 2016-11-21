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


import java.security.InvalidParameterException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

public final class AssertionFactoryManager {

  private final AtomicBoolean assertionSwitch;

  private final AtomicReference<AssertionFactory.Strategy> strategyReference;

  private final AtomicReference<Function<String, ? extends RuntimeException>> exceptionMapperReference;

  private volatile AssertionFactory currentFactory;

  AssertionFactoryManager(Function<String, ? extends RuntimeException> exceptionMapper) {

    if (exceptionMapper == null) {
      throw new InvalidParameterException("'exceptionMapper' cannot be null.");
    }

    assertionSwitch = new AtomicBoolean(true);
    strategyReference = new AtomicReference<>(AssertionFactory.Strategy.THREAD_LOCAL);
    exceptionMapperReference = new AtomicReference<>(exceptionMapper);
    currentFactory = createFactory(strategyReference.get(), exceptionMapper);
  }
  
  public AssertionFactory getCurrentFactory() {
    
    return currentFactory;
  }

  public void setStrategy(AssertionFactory.Strategy strategy) {

    if (strategy == null) {
      throw new InvalidParameterException("'strategy' cannot be null.");
    }
    
    if (strategyReference.getAndSet(strategy) != strategy) {
      currentFactory = createFactory(strategy, exceptionMapperReference.get());
    }
  }

  public void setExceptionMapper(Function<String, ? extends RuntimeException> exceptionMapper) {

    if (exceptionMapper == null) {
      throw new InvalidParameterException("'exceptionMapper' cannot be null.");
    }
    
    if (exceptionMapperReference.getAndSet(exceptionMapper) != exceptionMapper) {
      currentFactory = createFactory(strategyReference.get(), exceptionMapper);
    }
  }

  public void switchOn() {

    assertionSwitch.set(true);
  }
  
  public void switchOff() {

    assertionSwitch.set(false);
  }

  private AssertionFactory createFactory(AssertionFactory.Strategy strategy,
                                         Function<String, ? extends RuntimeException> exceptionMapper) {

    AssertionFactory assertionFactory;
    if (strategy == AssertionFactory.Strategy.THREAD_LOCAL) {
      assertionFactory = Assertions.createThreadLocalExpectedCases(exceptionMapper);
    } else {
      assertionFactory = Assertions.createDefaultExpectedCases(exceptionMapper);
    }
    return Assertions.createSwitchableProxy(assertionFactory, assertionSwitch);
  }
}