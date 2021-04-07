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
 * The interface simulates a stop watch. For single threaded (or equivalent environment) use only. A typical sample use
 * case could be FPS rendering.
 */
public interface StopWatch {

  /**
   * Sets the tick in nano seconds. A tick is a time unit that is used to measure time expiration since anchor, and
   * to measure the amount of time for the anchor to move forward or backward.
   *
   * @param tickNanos the tick in nano seconds, must be greater than {@code 0} .
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code tickNanos} is less than or equal to
   *                                                                    {@code 0} .
   */
  void setTick(long tickNanos);

  /**
   * Queries the current tick in nano seconds.
   *
   * @return the current tick of the {@link StopWatch} in nano seconds.
   */
  long getTick();

  /**
   * Forward the anchor time by the amount of {@code getTick()} nano seconds, regardless of whether the
   * {@link StopWatch} is running or paused.
   *
   * @param currentNanos the current system time stamp in nano seconds.
   */
  void tickForward(long currentNanos);

  /**
   * Backward the anchor time by the amount of {@code getTick()} nano seconds, regardless of whether the
   * {@link StopWatch} is running or paused.
   *
   * @param currentNanos the current system time stamp in nano seconds.
   */
  void tickBackward(long currentNanos);

  /**
   * Given the current system timestamp in nano seconds, return whether the elapsed time since anchor is greater than or
   * equal to tick amount. Elapsed time is {@code currentNanos - anchor} when the watch is running, or the saved
   * snapshot elapsed amount if the watch is paused.
   *
   * @param currentNanos the current system time stamp in nano seconds.
   * @return {@code true} if the current tick is expired.
   */
  boolean expired(long currentNanos);

  /**
   * Sets the anchor time so that it will expire in {@code expiresInNanos} amount of time, regardless of whether the
   * {@link StopWatch} is currently running or paused. If paused, the {@link StopWatch} will expire in given
   * {@code expriresInNanos} after it's resumed running.
   *
   * Use negative {@code expiresInNanos} value to set the watch to an already-expired state.
   *
   * Notice that the method will ONLY change the anchor time and will NOT touch any other state, e.g. tick amount, or
   * saved snapshot elapsed time when watch is paused.
   *
   * @param expiresInNanos the amount of time in nano seconds, that the {@link StopWatch} will expire in.
   * @param currentNanos   the current system time stamp in nano seconds.
   */
  void set(long expiresInNanos, long currentNanos);

  /**
   * This method returns the amount of time elapsed (in nano seconds) since the anchor time, regardless of whether the
   * {@link StopWatch} is running or paused. In case the watch is paused, it will return the saved snapshot elapsed time
   * when it was paused.
   *
   * @param currentNanos the current system time stamp in nano seconds.
   * @return the amount of time elapsed in nano seconds, can be negative value in case the anchor time is set to some
   * time in the future (can be done with {@link #set(long, long)}) .
   */
  long readElapsed(long currentNanos);

  /**
   * This method returns the amount of time expired (in nano seconds), based on the elapsed time and tick amount. It is
   * a negative value when tick not expired: {@code elapsedTime < tick}.
   *
   * @param currentNanos the current system time stamp in nano seconds.
   * @return the amount of time expired in nano seconds, can be negative value in case the watch is not expired.
   */
  long readExpired(long currentNanos);

  /**
   * Set the anchor time to specified {@code currentNanos}; set the watch to be running.
   *
   * @param currentNanos the current system time stamp in nano seconds.
   */
  void reset(long currentNanos);

  /**
   * Pause the watch so that it no longer measures any more time elapsed, save a snapshot of already elapsed time with
   * value {@code currentNanos - anchor}.
   *
   * @param currentNanos the current system time stamp in nano seconds.
   */
  void pause(long currentNanos);

  /**
   * Resume the watch so that it starts measuring elapsed time again.
   *
   * @param currentNanos the current system time stamp in nano seconds.
   */
  void resume(long currentNanos);
}
