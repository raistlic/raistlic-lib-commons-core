package org.raistlic.common.stopwatch;

import org.raistlic.common.precondition.Precondition;

final class StopWatchDefault implements StopWatch {

  private long startedNanos;

  private long savedNanos;

  private long tickNanos;

  private boolean running;

  StopWatchDefault(long tickNanos) {

    Precondition.param(tickNanos).greaterThan(0L);
    this.tickNanos = tickNanos;
  }

  @Override
  public void setTick(long tickNanos) {

    Precondition.param(tickNanos).greaterThan(0L);

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

    if( running ) {
      // because when it expires: started + tick == current + expiresIn
      startedNanos = currentNanos + expiresInNanos - getTick();
    }
    else {
      // again when it expires: (current - saved) + tick == current + expiresIn
      savedNanos = getTick() - expiresInNanos;
    }
  }

  @Override
  public long readElapsed(long currentNanos) {

    return running ? currentNanos - startedNanos : savedNanos;
  }

  @Override
  public long readExpired(long currentNanos) {

    return readElapsed(currentNanos) - tickNanos;
  }

  @Override
  public void reset(long currentNanos) {

    startedNanos = currentNanos;
    running = true;
  }

  @Override
  public void pause(long currentNanos) {

    if( running ) {
      savedNanos = currentNanos - startedNanos;
      running = false;
    }
  }

  @Override
  public void resume(long currentNanos) {

    if( !running ) {
      startedNanos = currentNanos - savedNanos;
      running = true;
    }
  }
}
