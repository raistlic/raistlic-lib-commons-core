package org.raistlic.common.taskqueue;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.raistlic.common.util.ExceptionHandler;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.function.Consumer;

import static org.fest.assertions.api.Assertions.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

/**
 * Most of the thread-safety related aspects of the implementation are not covered in this test.
 *
 * @author Lei Chen (2015-12-02)
 */
@RunWith(MockitoJUnitRunner.class)
public class DefaultPromiseTest {

  @Mock
  private Task<Object> task;

  @Mock
  private ExceptionHandler exceptionHandler;

  @InjectMocks
  private DefaultPromise<Object> defaultPromise;

  @Test
  public void smockTest() {

    assertThat(defaultPromise).isNotNull();
  }

  @Test
  public void isNotDoneWhenCreated() {

    assertThat(defaultPromise.isDone()).isFalse();
  }

  @Test
  public void isDoneAfterRun() {

    defaultPromise.run();
    assertThat(defaultPromise.isDone()).isTrue();
  }

  @Test
  public void cancelExpected() {

    // can be canceled, when not done
    assertThat(defaultPromise.isCancelled()).isFalse();
    assertThat(defaultPromise.cancel(true)).isTrue();
    assertThat(defaultPromise.isCancelled()).isTrue();

    // canceled returns false when already canceled
    assertThat(defaultPromise.cancel(true)).isFalse();
    assertThat(defaultPromise.isCancelled()).isTrue();
  }

  @Test
  public void cancelAlreadyDonePromiseReturnsFalse() {

    defaultPromise.run();
    assertThat(defaultPromise.cancel(true)).isFalse();
    assertThat(defaultPromise.isCancelled()).isFalse();
    assertThat(defaultPromise.isDone()).isTrue();
  }

  @Test
  public void getExpected() throws Exception {

    Runnable runnable = new Runnable() {

      @Override
      public void run() {

        try {
          Thread.sleep(1000);
          defaultPromise.run();
        }
        catch (Exception ex) {
          throw new RuntimeException(ex);
        }
      }
    };
    Object result = new Object();
    when(task.run()).thenReturn(result);

    new Thread(runnable).start();

    Object actual = defaultPromise.get();
    assertThat(actual).isEqualTo(result);
    verify(task, times(1)).run();
  }

  @Test(expected = TimeoutException.class)
  public void getWhenTimeout() throws Exception {

    final CountDownLatch cd = new CountDownLatch(1);
    try {
      Runnable runnable = new Runnable() {

        @Override
        public void run() {

          try {
            cd.await();
            defaultPromise.run();
          }
          catch (Exception ex) {
            throw new RuntimeException(ex);
          }
        }
      };
      new Thread(runnable).start();
      defaultPromise.get(500, TimeUnit.MILLISECONDS);
    }
    finally {
      cd.countDown();
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void onResultCalledWhenTaskSucceeds() {

    Object result = new Object();
    when(task.run()).thenReturn(result);

    Consumer<Object> consumer = mock(Consumer.class);
    defaultPromise.onResult(consumer);

    defaultPromise.run();

    verify(consumer, times(1)).accept(result);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void onResultNotCalledWhenTaskThrowsException() {

    when(task.run()).thenThrow(new TaskExecutionException(null));

    Consumer<Object> consumer = mock(Consumer.class);
    defaultPromise.onResult(consumer);

    defaultPromise.run();

    verifyZeroInteractions(consumer);
  }

  @Test
  public void onErrorCalledWhenTaskThrowsException() {

    TaskExecutionException exception = new TaskExecutionException(null);
    when(task.run()).thenThrow(exception);

    ExceptionHandler errorHandler = mock(ExceptionHandler.class);
    defaultPromise.onError(errorHandler);

    defaultPromise.run();

    verify(errorHandler, times(1)).exceptionOccur(any(Thread.class), eq(exception));
  }

  @Test
  public void onErrorNotCalledWhenTaskSucceeds() {

    when(task.run()).thenReturn(new Object());

    ExceptionHandler errorHandler = mock(ExceptionHandler.class);
    defaultPromise.onError(errorHandler);

    defaultPromise.run();

    verifyZeroInteractions(errorHandler);
  }
}
