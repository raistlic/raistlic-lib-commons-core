/*
 * Copyright 2013 Lei CHEN (raistlic@gmail.com)
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
 *
 * @author Lei.C (2013-03-27)
 */
public interface StopWatch {
  
  /**
   * System call, get the current time, represented in the implementation 
   * designated precision, as a long integer.
   * 
   * @return the current time.
   */
  public long current();
  
  public void setTick(long tick);
  
  public long getTick();
  
  public void tick(long current);
  
  public boolean expired(long current);
  
  /**
   * This method sets the StopWatch's "start timing time", so that it will expire 
   * after the specified {@code time}, notice that, if {@code time} is minus, 
   * then it means the StopWatch has already expired(for {@code -time} long now).
   * 
   * <p/>
   * 
   * @param time
   * 
   * @param current the current system time, which precision has to match the 
   *        StopWatch precision, otherwise all queries against the StopWatch does
   *        not make any sense.
   */
  public void set(long time, long current);
  
  /**
   * This method returns the amount of time eclipsed after the StopWatch started
   * timing.
   * 
   * @param current the current system time, which precision has to match the 
   *        StopWatch precision, otherwise all queries against the StopWatch does
   *        not make any sense.
   * 
   * @return the amount of time eclipsed after the StopWatch started
   *         timing.
   */
  public long read(long current);
  
  /**
   * The effect of this method is equal to the combination of the following two
   * calls:
   * 
   * <pre>
   * set(getTick(), current);
   * resume(current);
   * </pre>
   * 
   * @param current the current system time, which precision has to match the 
   *        StopWatch precision, otherwise all queries against the StopWatch does
   *        not make any sense.
   */
  public void reset(long current);
  
  public void pause(long current);
  
  public void resume(long current);
}
