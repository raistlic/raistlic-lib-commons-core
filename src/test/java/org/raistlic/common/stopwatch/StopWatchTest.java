package org.raistlic.common.stopwatch;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import org.raistlic.common.precondition.InvalidParameterException;

import java.util.concurrent.TimeUnit;

import static org.fest.assertions.api.Assertions.assertThat;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;

/**
 * @author Lei Chen (2016-01-21)
 */
@RunWith(JUnit4.class)
public class StopWatchTest {

  private StopWatch.TimeStrategy timeStrategy;

  private StopWatch stopWatch;

  private long startingTime;

  private final long defaultTickNano = TimeUnit.MICROSECONDS.toNanos(16L);

  @Before
  public void setup() {

    timeStrategy = spy(new NanoTimeStrategy());
    timeStrategy.markCurrent();
    startingTime = timeStrategy.getMarkedCurrent();
    stopWatch = StopWatchFactory.newFactory(timeStrategy)
            .withTick(defaultTickNano, TimeUnit.NANOSECONDS)
            .get();
  }

  @Test(expected = InvalidParameterException.class)
  public void setTickWithNullTimeUnit() {

    stopWatch.setTick(1L, null);
  }

  @Test(expected = InvalidParameterException.class)
  public void setTickWithZeroTickAmount() {

    stopWatch.setTick(0L, TimeUnit.MILLISECONDS);
  }

  @Test(expected = InvalidParameterException.class)
  public void setTickWithNegativeTickAmount() {

    stopWatch.setTick(-123L, TimeUnit.MILLISECONDS);
  }

  @Test
  public void setAndGetTickExpected() {

    assertThat(stopWatch.getTick(TimeUnit.NANOSECONDS)).isEqualTo(defaultTickNano);
    stopWatch.setTick(10L, TimeUnit.SECONDS);
    assertThat(stopWatch.getTick(TimeUnit.SECONDS)).isEqualTo(10L);
    assertThat(stopWatch.getTick(TimeUnit.MILLISECONDS)).isEqualTo(10000L);
    assertThat(stopWatch.getTick(TimeUnit.MINUTES)).isEqualTo(0L);
  }

  @Test
  public void isCurrentTickExpiredExpected() {

    // when 0 nano seconds passed
    assertThat(stopWatch.isCurrentTickExpired()).isFalse();

    // when 1 nano seconds passed
    doReturn(startingTime + 1L).when(timeStrategy).getMarkedCurrent();
    assertThat(stopWatch.isCurrentTickExpired()).isFalse();

    // when tick - 1 nano seconds passed
    doReturn(startingTime + (defaultTickNano - 1L)).when(timeStrategy).getMarkedCurrent();
    assertThat(stopWatch.isCurrentTickExpired()).isFalse();

    // when tick nano seconds passed
    doReturn(startingTime + defaultTickNano).when(timeStrategy).getMarkedCurrent();
    assertThat(stopWatch.isCurrentTickExpired()).isTrue();

    // when tick + 1 nano seconds passed
    doReturn(startingTime + defaultTickNano + 1L).when(timeStrategy).getMarkedCurrent();
    assertThat(stopWatch.isCurrentTickExpired()).isTrue();

    // when 1 day passed
    doReturn(startingTime + TimeUnit.DAYS.toNanos(1L)).when(timeStrategy).getMarkedCurrent();
    assertThat(stopWatch.isCurrentTickExpired()).isTrue();
  }

  @Test
  public void readExpected() {

    // when 0 nano seconds passed
    assertThat(stopWatch.read(TimeUnit.NANOSECONDS)).isEqualTo(0L);

    // when 10 milli-seconds passed
    doReturn(startingTime + TimeUnit.MILLISECONDS.toNanos(10L)).when(timeStrategy).getMarkedCurrent();
    assertThat(stopWatch.read(TimeUnit.MILLISECONDS)).isEqualTo(10L);

    // when 123 minutes passed
    doReturn(startingTime + TimeUnit.MINUTES.toNanos(123L)).when(timeStrategy).getMarkedCurrent();
    assertThat(stopWatch.read(TimeUnit.MINUTES)).isEqualTo(123L);
    assertThat(stopWatch.read(TimeUnit.SECONDS)).isEqualTo(123L * 60);

    // when 4 days passed
    doReturn(startingTime + TimeUnit.DAYS.toNanos(4L)).when(timeStrategy).getMarkedCurrent();
    assertThat(stopWatch.read(TimeUnit.DAYS)).isEqualTo(4L);
    assertThat(stopWatch.read(TimeUnit.HOURS)).isEqualTo(4L * 24);
  }

  @Test
  public void tickForwardExpected() {

    stopWatch.tickForward();
    assertThat(stopWatch.read(TimeUnit.NANOSECONDS)).isEqualTo(-defaultTickNano);

    stopWatch.tickForward();
    assertThat(stopWatch.read(TimeUnit.NANOSECONDS)).isEqualTo(-2 * defaultTickNano);

    stopWatch.tickForward();
    assertThat(stopWatch.read(TimeUnit.NANOSECONDS)).isEqualTo(-3 * defaultTickNano);
  }

  @Test
  public void tickBackwardExpected() {

    stopWatch.tickBackward();
    assertThat(stopWatch.read(TimeUnit.NANOSECONDS)).isEqualTo(defaultTickNano);

    stopWatch.tickBackward();
    assertThat(stopWatch.read(TimeUnit.NANOSECONDS)).isEqualTo(2 * defaultTickNano);

    stopWatch.tickBackward();
    assertThat(stopWatch.read(TimeUnit.NANOSECONDS)).isEqualTo(3 * defaultTickNano);
  }

  @Test
  public void getTimeStrategyExpected() {

    assertThat(stopWatch.getTimeStrategy()).isEqualTo(timeStrategy);
  }
}
