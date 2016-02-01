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

package org.raistlic.common.stopwatch;

import org.raistlic.common.precondition.Precondition;
import org.raistlic.common.util.Factory;

import java.util.concurrent.TimeUnit;

/**
 *
 * @author Lei CHEN (2013-03-27)
 * @since 1.0
 */
public final class StopWatchFactory implements Factory<StopWatch> {

  public static StopWatch createStopWatch(StopWatch.TimeStrategy timeStrategy,
                                          long tickAmount,
                                          TimeUnit timeUnit) {

    Precondition.param(timeStrategy, "timeStrategy").notNull();
    Precondition.param(tickAmount, "tickAmount").greaterThan(0L);
    Precondition.param(timeUnit, "timeUnit").notNull();

    return doCreateStopWatch(timeStrategy, tickAmount, timeUnit);
  }

  public static StopWatchFactory newFactory(StopWatch.TimeStrategy timeStrategy) {

    Precondition.param(timeStrategy, "timeStrategy").notNull();
    return new StopWatchFactory(timeStrategy);
  }

  public static StopWatchFactory newNanoWatchFactory() {

    return newFactory(newNanoTimeStrategy());
  }

  public static StopWatch.TimeStrategy newNanoTimeStrategy() {

    return new NanoTimeStrategy();
  }

  private final StopWatch.TimeStrategy timeStrategy;

  private long tickAmount;

  private TimeUnit timeUnit;

  private StopWatchFactory(StopWatch.TimeStrategy timeStrategy) {

    this.timeStrategy = timeStrategy;
    this.tickAmount = 1L;
    this.timeUnit = TimeUnit.SECONDS;
  }

  public StopWatchFactory withTick(long tickAmount, TimeUnit timeUnit) {

    Precondition.param(tickAmount, "tickAmount").greaterThan(0L);
    Precondition.param(timeUnit, "timeUnit").notNull();

    this.tickAmount = tickAmount;
    this.timeUnit = timeUnit;
    return this;
  }

  public StopWatch.TimeStrategy getTimeStrategy() {

    return timeStrategy;
  }

  @Override
  public StopWatch get() {

    return doCreateStopWatch(timeStrategy, tickAmount, timeUnit);
  }

  private static StopWatch doCreateStopWatch(StopWatch.TimeStrategy timeStrategy,
                                             long tickAmount,
                                             TimeUnit timeUnit) {

    return new DefaultStopWatch(timeStrategy, tickAmount, timeUnit);
  }
}
