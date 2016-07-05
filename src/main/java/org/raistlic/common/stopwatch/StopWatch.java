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

/**
 * The interface simulates a stop watch. For single threaded (or equivalent environment) use only.
 *
 * @author Lei Chen (2016-01-20)
 */
public interface StopWatch {

  /**
   * Sets the tick in nano seconds. A tick is an amount of expiration time that's repeatedly used in the stop watch.
   *
   * @param tickNanos the tick in nano seconds, must be greater than {@code 0} .
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code tickNanos} is less than or equal to
   *         {@code 0} .
   */
  void setTick(long tickNanos);

  /**
   * Queries the current tick in nano seconds.
   *
   * @return the current tick of the {@link StopWatch} in nano seconds.
   */
  long getTick();

  /**
   * Forward the marked start time by the amount of {@code getTick()} nano seconds, regardless of whether the
   * {@link StopWatch} is paused or not.
   *
   * @param currentNanos the current system time stamp in nano seconds.
   */
  void tickForward(long currentNanos);

  /**
   * Change the marked start time backward by the amount of {@code getTick()} nano seconds, regardless of whether the
   * {@link StopWatch} is paused or not.
   *
   * @param currentNanos the current system time stamp in nano seconds.
   */
  void tickBackward(long currentNanos);

  /**
   * Given the current system time stamp in nano seconds, return whether the current tick is expired.
   *
   * @param currentNanos the current system time stamp in nano seconds.
   * @return {@code true} if the current tick is expired.
   */
  boolean expired(long currentNanos);
  
  /**
   * Sets the {@link StopWatch}'s marked start time so that it will expire in {@code expiresInNanos} amount of time,
   * regardless of whether the {@link StopWatch} is currently paused or not. If paused, the {@link StopWatch} will expire
   * in {@code expriresInNanos} nano seconds after it's resumed. Notice that the method does not change the watch's tick
   * amount.
   *
   * Use negative {@code expiresInNanos} value to set the watch to an already-expired state.
   *
   * @param expiresInNanos the amount of time in nano seconds, that the {@link StopWatch} will expire in.
   * @param currentNanos the current system time stamp in nano seconds.
   */
  void set(long expiresInNanos, long currentNanos);
  
  /**
   * This method returns the amount of time elapsed (in nano seconds) since the latest marked start time, comparing
   * with either the given {@code currentNanos} or the moment when the watch was paused, depending on whether the watch
   * is running or being paused, respectively.
   *
   * See also {@link #reset(long)}.
   *
   * @param currentNanos the current system time stamp in nano seconds.
   * @return the amount of time elapsed in nano seconds, can be negative value in case the marked start time is some
   *         time in the future.
   */
  long readElapsed(long currentNanos);

  /**
   * This method returns the amount of time expired (in nano seconds), based on the latest marked start time and tick
   * amount, comparing with either the given {@code currentNanos} or the moment when the watch was paused, depending on
   * whether the watch is running or being paused, respectively.
   *
   * @param currentNanos the current system time stamp in nano seconds.
   * @return the amount of time expired in nano seconds, can be negative value in case the watch is not expired.
   */
  long readExpired(long currentNanos);
  
  /**
   * Set the specified current time as the marked start time; set the watch to be running.
   *
   * @param currentNanos the current system time stamp in nano seconds.
   */
  void reset(long currentNanos);

  /**
   * Pause the watch so that it no longer measures any more time elapsed.
   * 
   * @param currentNanos the current system time stamp in nano seconds.
   */
  void pause(long currentNanos);

  /**
   * Resume the watch so that it start measuring elapsed time again.
   *
   * @param currentNanos the current system time stamp in nano seconds.
   */
  void resume(long currentNanos);
}
