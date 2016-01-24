/*
 * Copyright 2016 Lei CHEN (raistlic@gmail.com)
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

import java.util.concurrent.TimeUnit;

/**
 * @author Lei Chen (2016-01-20)
 */
public interface StopWatch {

  void setTick(long tickAmount, TimeUnit timeUnit);

  long getTick(TimeUnit timeUnit);

  boolean isCurrentTickExpired();

  int getTickExpiredTimes();

  void tickForward();

  void tickBackward();

  void setExpireIn(long expireInAmount, TimeUnit timeUnit);

  long read(TimeUnit timeUnit);

  void reset();

  void pause();

  void resume();

  boolean isPaused();

  TimeStrategy getTimeStrategy();

  interface TimeStrategy {

    void markCurrent();

    long getMarkedCurrent();

    long toAbsoluteAmount(long timeUnitAmount, TimeUnit timeUnit);

    long toTimeUnitAmount(long absoluteAmount, TimeUnit timeUnit);
  }
}
