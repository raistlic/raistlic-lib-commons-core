package org.raistlic.common.stopwatch;

import java.util.concurrent.TimeUnit;

/**
 * The only reason why this is not a singleton is because it's stateful & not thread-safe. The class
 * (and the whole StopWatch package) is pretty much designed for GUI use, thus is to work (fast)
 * in a single-threaded environment. Also it doesn't implement any ThreadLocal based instance
 * holders, simply because the instantiation & instance management policy is up to the user.
 *
 * @author Lei Chen (2016-01-20)
 */
class NanoTimeStrategy implements StopWatch.TimeStrategy {

  private long markedCurrent;

  @Override
  public void markCurrent() {

    markedCurrent = System.nanoTime();
  }

  @Override
  public long getMarkedCurrent() {

    return markedCurrent;
  }

  @Override
  public long toAbsoluteAmount(long timeUnitAmount, TimeUnit timeUnit) {

    return timeUnit.toNanos(timeUnitAmount);
  }

  @Override
  public long toTimeUnitAmount(long absoluteAmount, TimeUnit timeUnit) {

    return timeUnit.convert(absoluteAmount, TimeUnit.NANOSECONDS);
  }
}
