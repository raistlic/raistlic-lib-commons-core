package org.raistlic.common.stopwatch;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import java.util.concurrent.TimeUnit;

import static org.fest.assertions.api.Assertions.assertThat;

/**
 * @author Lei Chen (2016-01-21)
 */
@RunWith(JUnit4.class)
public class TimeStrategyTest {

  private StopWatch.TimeStrategy timeStrategy;

  @Before
  public void setup() {

    timeStrategy = new NanoTimeStrategy();
  }

  @Test
  public void markCurrentExpected() throws Exception {

    timeStrategy.markCurrent();
    long nanoTime = timeStrategy.getMarkedCurrent();

    for (int i = 0; i < 10; i++) {
      Thread.sleep(1L);
      assertThat(timeStrategy.getMarkedCurrent()).isEqualTo(nanoTime);
      assertThat(timeStrategy.getMarkedCurrent()).isNotEqualTo(System.nanoTime());
    }
  }

  @Test
  public void toAbsoluteAmountExpected() {

    assertThat(timeStrategy.toAbsoluteAmount(1L, TimeUnit.SECONDS)).isEqualTo(1000000000L);
    assertThat(timeStrategy.toAbsoluteAmount(2L, TimeUnit.MILLISECONDS)).isEqualTo(2000000L);
    assertThat(timeStrategy.toAbsoluteAmount(3L, TimeUnit.MINUTES)).isEqualTo(180000000000L);
  }

  @Test
  public void toTimeUnitAmountExpected() {

    assertThat(timeStrategy.toTimeUnitAmount(24000012340L, TimeUnit.SECONDS)).isEqualTo(24L);
    assertThat(timeStrategy.toTimeUnitAmount(654321012L, TimeUnit.MILLISECONDS)).isEqualTo(654L);
    assertThat(timeStrategy.toTimeUnitAmount(123456700000L, TimeUnit.MINUTES)).isEqualTo(2L);
  }
}
