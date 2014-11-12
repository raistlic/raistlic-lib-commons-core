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
abstract class AbstractStopWatch implements StopWatch {
  
  private long started;
  private long saved;
  private long target;
  private boolean running;
  
  AbstractStopWatch() {
    
    this.target = 0L;
    reset(current());
  }
  
  @Override
  public final void setTick(long tick) {
    
    this.target = tick;
  }

  @Override
  public final long getTick() {
    
    return this.target;
  }

  @Override
  public final void tick(long current) {
    
    /*
     * this method shifts the StopWatch's "started timing time", any operation 
     * that shifts the start time can be done by calling set(?, current).
     * 
     * by calling 
     *   set(?, current)
     * we want to acheive that, in case the StopWatch is running:
     *   started += getTick();
     * otherwise if the StopWatch is paused:
     *   saved -= getTick();
     * 
     * if the StopWatch is running, to make it work:
     *   set(?, current) === started = started + getTick();
     * which means,
     *   started = current + ? - getTick(); 
     *   === 
     *   started = started + getTick();
     * so we just have to make sure:
     *   current + ? - getTick() == started + getTick();
     *   ? == started + getTick() + getTick() - current;
     *   ? == getTick() * 2 - (current - started);
     *   ? == getTick() * 2 - read(current);
     * 
     * so otherwise if the StopWatch is paused, how does it work?
     *   ? == getTick() * 2 - read(current);
     *   ? == getTick() * 2 - saved;
     *   set(?, current);
     *   saved = getTick() - ?;
     *   saved = getTick() - (getTick() * 2 - saved);
     *   saved = saved - getTick();
     * which is what we want to acheive :-)
     */
    set(getTick() * 2 - read(current), current);
  }

  @Override
  public final boolean expired(long current) {
    
    return read(current) >= target;
  }

  @Override
  public final void set(long time, long current) {
    
    if( running ) {

      /*
       * say, if the StopWatch expires now, then: 
       * 
       *   started + getTick() == current
       * 
       * so, to make the StopWatch expire after 'time', then we just need to 
       * assign value to 'started', so that the following stands:
       * 
       *   started + getTick() == current + time
       */
      started = current + time - getTick();
    }
    else {
      
      /*
       * when being paused, we have:
       * 
       *   saved == current - started;
       * 
       * in other words, when resuming we will have:
       *   
       *   started == current - saved;
       * 
       * so to make sure that after resume running we get the currect 'started'
       * value, we just need to change 'saved' so that:
       * 
       *   (current - saved) + getTick() == current + time;
       */
      saved = getTick() - time;
    }
  }

  @Override
  public final long read(long current) {
    
    return running ? current - started : saved;
  }

  @Override
  public final void reset(long current) {
    
    started = current;
    running = true;
  }

  @Override
  public final void pause(long current) {
    
    if( running ) {
      
      saved = current - started;
      running = false;
    }
  }

  @Override
  public final void resume(long current) {
    
    if( !running ) {
      
      started = current - saved;
      running = true;
    }
  }
}
