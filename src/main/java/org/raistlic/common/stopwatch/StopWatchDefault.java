package org.raistlic.common.stopwatch;

import org.raistlic.common.precondition.Param;

/**
 * Default implementation for {@link StopWatch} .
 */
final class StopWatchDefault implements StopWatch {

  /**
   * The mark of "start time".
   */
  private long anchor;

  /**
   * The saved elapsed time, calculated on pausing the watch, and only used when watch is paused.
   */
  private long elapsedAmount;

  /**
   * Expiration amount, also used as a unit for moving anchor forward and backward.
   */
  private long tickNanos;

  /**
   * true indicating the watch is actively measuring time elapsed.
   */
  private boolean running;

  StopWatchDefault(long tickNanos) {

    Param.isTrue(tickNanos > 0L, "tickNanos must be greater than 0");

    this.tickNanos = tickNanos;
  }

  @Override
  public void setTick(long tickNanos) {

    Param.isTrue(tickNanos > 0L, "tickNanos must be greater than 0");

    this.tickNanos = tickNanos;
  }

  @Override
  public long getTick() {

    return this.tickNanos;
  }

  @Override
  public void tickForward(long current) {

    // assume set(?, current) will achieve the goal, then
    //
    // when running, goal is : started = started + getTick()
    // started + getTick() = current + ? - getTick() -> ? = getTick()*2 - (current - started) = getTick()*2 - readElapsed(current)
    // when saved, goal is : saved = saved - getTick()
    // getTick() - ? = saved - getTick() -> ? = getTick()*2 - saved = getTick()*2 - readElapsed(current)
    set(getTick() * 2 - readElapsed(current), current);
  }

  @Override
  public void tickBackward(long currentNanos) {

    // assume set(?, current) will achieve the goal, then
    //
    // when running, goal is : started = started - getTick()
    // started = started - getTick() -> started - getTick() = current + ? - getTick() -> ? = started - current = -readElapsed(current)
    // when saved, goal is : saved = saved + getTick()
    // saved = saved + getTick() -> getTick() - ? = saved + getTick() -> ? = -saved = -readElapsed(current)
    set(-readElapsed(currentNanos), currentNanos);
  }

  @Override
  public boolean expired(long currentNanos) {

    return readElapsed(currentNanos) >= tickNanos;
  }

  @Override
  public void set(long expiresInNanos, long currentNanos) {

    if (running) {
      // because when it expires: started + tick == current + expiresIn
      anchor = currentNanos + expiresInNanos - tickNanos;
    } else {
      // again when it expires: (current - saved) + tick == current + expiresIn
      elapsedAmount = tickNanos - expiresInNanos;
    }
  }

  @Override
  public long readElapsed(long currentNanos) {

    return running ? currentNanos - anchor : elapsedAmount;
  }

  @Override
  public long readExpired(long currentNanos) {

    return readElapsed(currentNanos) - tickNanos;
  }

  @Override
  public void reset(long currentNanos) {

    anchor = currentNanos;
    running = true;
  }

  @Override
  public void pause(long currentNanos) {

    if (running) {
      elapsedAmount = currentNanos - anchor;
      running = false;
    }
  }

  @Override
  public void resume(long currentNanos) {

    if (!running) {
      anchor = currentNanos - elapsedAmount;
      running = true;
    }
  }
}
