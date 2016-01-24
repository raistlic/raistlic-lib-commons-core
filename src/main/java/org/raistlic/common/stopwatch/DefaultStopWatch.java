package org.raistlic.common.stopwatch;

import org.raistlic.common.precondition.Precondition;

import java.util.concurrent.TimeUnit;

/**
 * @author Lei Chen (2016-01-21)
 */
class DefaultStopWatch implements StopWatch {

  private final TimeStrategy timeStrategy;

  private long marked;

  private long tick;

  private long savedRead;

  private boolean paused;

  DefaultStopWatch(TimeStrategy timeStrategy, long tickAmount, TimeUnit timeUnit) {

    assert timeStrategy != null;
    assert tickAmount > 0;
    assert timeUnit != null;

    this.timeStrategy = timeStrategy;
    this.marked = timeStrategy.getMarkedCurrent();
    this.tick = timeStrategy.toAbsoluteAmount(tickAmount, timeUnit);
  }

  @Override
  public void setTick(long tickAmount, TimeUnit timeUnit) {

    Precondition.assertParam(tickAmount > 0L, "'tickAmount' must be greater than 0");
    Precondition.assertParam(timeUnit != null, "'timeUnit' cannot be null.");

    this.tick = timeStrategy.toAbsoluteAmount(tickAmount, timeUnit);
  }

  @Override
  public long getTick(TimeUnit timeUnit) {

    Precondition.assertParam(timeUnit != null, "'timeUnit' cannot be null.");

    return timeStrategy.toTimeUnitAmount(tick, timeUnit);
  }

  @Override
  public boolean isCurrentTickExpired() {

    return readAbsolute() >= tick;
  }

  @Override
  public int getTickExpiredTimes() {

    return (int) (readAbsolute() / tick);
  }

  @Override
  public void tickForward() {

    marked += tick;
  }

  @Override
  public void tickBackward() {

    marked -= tick;
  }

  @Override
  public void setExpireIn(long expireInAmount, TimeUnit timeUnit) {

    Precondition.assertParam(expireInAmount >= 0, "'expireInAmount' must be greater than 0");
    Precondition.assertParam(timeUnit != null, "'timeUnit' cannot be null.");

    long expireAbsolute = timeStrategy.toAbsoluteAmount(expireInAmount, timeUnit);
    if (paused) {
      savedRead = expireAbsolute - tick;
    } else {
      long currentAbsolute = timeStrategy.getMarkedCurrent();
      marked  = currentAbsolute + tick - expireAbsolute;
    }
  }

  @Override
  public long read(TimeUnit timeUnit) {

    Precondition.assertParam(timeUnit != null, "'timeUnit' cannot be null.");

    long readAbsolute = readAbsolute();
    return timeStrategy.toTimeUnitAmount(readAbsolute, timeUnit);
  }

  private long readAbsolute() {

    if (paused) {
      return savedRead;
    } else {
      return timeStrategy.getMarkedCurrent() - marked;
    }
  }

  @Override
  public void reset() {

    marked = timeStrategy.getMarkedCurrent();
    paused = false;
  }

  @Override
  public void pause() {

    if (paused) {
      return;
    }
    savedRead = readAbsolute();
    paused = true;
  }

  @Override
  public void resume() {

    if (!paused) {
      return;
    }
    long currentAbsolute = timeStrategy.getMarkedCurrent();
    marked = currentAbsolute - savedRead;
    paused = false;
  }

  @Override
  public boolean isPaused() {

    return paused;
  }

  @Override
  public TimeStrategy getTimeStrategy() {

    return timeStrategy;
  }
}
