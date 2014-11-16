package org.raistlic.common.event;

import org.junit.Test;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.raistlic.common.ExceptionHandler;

import java.lang.reflect.Method;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * @author lei.c
 * @since 2014-11-16
 */
public class EventChannelTest {

  /**
   * The method verifies that {@link EventChannel} dispatches events to all registered listeners.
   *
   * @throws Exception if unexpected error occurs which causes the test to fail.
   */
  @Test
  public void testRegisteredAdapterReceivesEvents() throws Exception {

    // prepare event listener adapter:
    MyListener listener = mock(MyListener.class);
    MyListener listener2 = mock(MyListener.class);
    Method method = MyListener.class.getDeclaredMethod("receiveEvent", Event.class);
    String name = "*";
    EventListenerAdapter adapter = new EventListenerAdapter(listener, method, Event.class, name);
    EventListenerAdapter adapter2 = new EventListenerAdapter(listener2, method, Event.class, name);

    // prepare event:
    Object source = new Object();
    Event event = mock(Event.class);
    when(event.getSource()).thenReturn(source);

    // mock up exception handler
    ExceptionHandler exceptionHandler = mock(ExceptionHandler.class);

    // create channel and register listener adapter:
    EventChannel eventChannel = new EventChannel(name, exceptionHandler);
    eventChannel.add(adapter);
    eventChannel.add(adapter2);

    // dispatch event:
    eventChannel.dispatch(event);

    // verify that both registered adapters received the event, and none exception occur:
    verify(listener, atLeastOnce()).receiveEvent(event);
    verify(listener2, atLeastOnce()).receiveEvent(event);
    verify(exceptionHandler, times(0)).exceptionOccur(any(Thread.class), any(Throwable.class));
  }

  /**
   * The method verifies that the un-registered listeners no longer receive events from the
   * {@link EventDispatcher} .
   *
   * @throws Exception unexpected error which caused the test to fail.
   */
  @Test
  public void testUnregisteredAdapterNoLongerReceivesEvents() throws Exception {

    // prepare event listener adapters:
    MyListener listener = mock(MyListener.class);
    MyListener listener2 = mock(MyListener.class);
    Method method = MyListener.class.getDeclaredMethod("receiveEvent", Event.class);
    String name = "*";
    EventListenerAdapter adapter = new EventListenerAdapter(listener, method, Event.class, name);
    EventListenerAdapter adapter2 = new EventListenerAdapter(listener2, method, Event.class, name);

    // prepare event:
    Object source = new Object();
    Event event = mock(Event.class);
    when(event.getSource()).thenReturn(source);

    // mock up exception handler
    ExceptionHandler exceptionHandler = mock(ExceptionHandler.class);

    // create channel and register listener adapter:
    EventChannel eventChannel = new EventChannel(name, exceptionHandler);
    eventChannel.add(adapter);
    eventChannel.add(adapter2);

    // dispatch event:
    eventChannel.dispatch(event);

    // verify that both registered adapters received the event, and none exception occur:
    verify(listener, atLeastOnce()).receiveEvent(event);
    verify(listener2, atLeastOnce()).receiveEvent(event);
    verify(exceptionHandler, times(0)).exceptionOccur(any(Thread.class), any(Throwable.class));

    // reset mocks & then unregister listener2:
    reset(listener);
    reset(listener2);
    reset(exceptionHandler);

    eventChannel.remove(listener);

    // dispatch event again:
    eventChannel.dispatch(event);

    // verify that unregistered listener no longer receives events:
    verify(listener, times(0)).receiveEvent(event);
    verify(listener2, atLeastOnce()).receiveEvent(event);
    verify(exceptionHandler, times(0)).exceptionOccur(any(Thread.class), any(Throwable.class));
  }

  @Test
  public void testEventChannelUsesExceptionHandlerToHandleExceptions() throws Exception {

    // prepare event listener adapters:
    MyListener listener = mock(MyListener.class);
    MyListener listener2 = mock(MyListener.class);
    Method method = MyListener.class.getDeclaredMethod("receiveEvent", Event.class);
    String name = "*";
    EventListenerAdapter adapter = new EventListenerAdapter(listener, method, Event.class, name);
    EventListenerAdapter adapter2 = new EventListenerAdapter(listener2, method, Event.class, name);

    // listener throws exception on event:
    doAnswer(new Answer<Void>() {

      @Override
      public Void answer(InvocationOnMock invocation) throws Throwable {

        throw new IllegalStateException("listener not happy.");
      }
    }).when(listener).receiveEvent(any(Event.class));

    // prepare event:
    Object source = new Object();
    Event event = mock(Event.class);
    when(event.getSource()).thenReturn(source);

    // mock up exception handler
    ExceptionHandler exceptionHandler = mock(ExceptionHandler.class);

    // create channel and register listener adapter:
    EventChannel eventChannel = new EventChannel(name, exceptionHandler);
    eventChannel.add(adapter);
    eventChannel.add(adapter2);

    // dispatch event:
    eventChannel.dispatch(event);

    // verifies that listener gets the event, and that exceptionHandler handles the exception:
    verify(listener, atLeastOnce()).receiveEvent(event);
    verify(exceptionHandler, times(1)).exceptionOccur(any(Thread.class), any(IllegalStateException.class));

    // meanwhile, listener2 should gets the event regardless:
    verify(listener2, atLeastOnce()).receiveEvent(event);
  }

  public static interface MyListener {

    public void receiveEvent(Event event);
  }
}
