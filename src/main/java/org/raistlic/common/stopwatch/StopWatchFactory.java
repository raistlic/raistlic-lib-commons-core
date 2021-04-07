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

import org.raistlic.common.precondition.Param;

import java.util.concurrent.TimeUnit;

/**
 * Factory class for creating {@link StopWatch} instances.
 */
public final class StopWatchFactory {

  /**
   * Create {@link StopWatch} instance with specified tick.
   *
   * @param tickAmount the tick amount, must be greater than {@code 0} .
   * @param tickUnit the tick time unit, cannot be {@code null}.
   * @return the {@link StopWatch} instance created.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when any of the parameters are invalid.
   */
  public static StopWatch createStopWatch(long tickAmount, TimeUnit tickUnit) {

    Param.isTrue(tickAmount > 0, "tickAmount must be greater than 0");
    Param.notNull(tickUnit, "tickUnit cannot be null");

    return new StopWatchDefault(tickUnit.toNanos(tickAmount));
  }

  private StopWatchFactory() {
  }
}
