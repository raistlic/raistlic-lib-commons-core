package org.raistlic.common.stopwatch;

import java.util.concurrent.TimeUnit;

/**
 * @author Lei Chen (2016-01-20)
 */
public interface TimerStrategy {

  long getCurrentTime();

  long translate(long amount, TimeUnit timeUnit);

  void refresh();
}
