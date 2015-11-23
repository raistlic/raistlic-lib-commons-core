package org.raistlic.common.precondition;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import org.raistlic.common.predicate.Predicates;

/**
 * @author Lei CHEN (2015-11-23)
 */
@RunWith(JUnit4.class)
public class GeneralParameterPreconditionTest {

  @Test(expected = InvalidParameterException.class)
  public void isNullWithNonNullParameter() {

    Precondition.param(new Object()).isNull();
  }

  @Test
  public void isNullWithNullParameter() {

    Precondition.param((Object)null).isNull();
  }

  @Test
  public void notNullWithNonNullParameter() {

    Precondition.param(new Object()).notNull();
  }

  @Test(expected = InvalidParameterException.class)
  public void notNullWithNullParameter() {

    Precondition.param((Object) null).notNull();
  }

  @Test
  public void equalToWithEqualTarget() {

    Object object = new Object();
    Precondition.param(object).equalTo(object);
  }

  @Test(expected = InvalidParameterException.class)
  public void equalToWithNotEqualTarget() {

    Precondition.param(new Object()).equalTo(new Object());
  }

  @Test(expected = InvalidParameterException.class)
  public void notEqualToWithEqualTarget() {

    Object object = new Object();
    Precondition.param(object).notEqualTo(object);
  }

  @Test
  public void notEqualToWithNotEqualTarget() {

    Precondition.param(new Object()).notEqualTo(new Object());
  }

  @Test
  public void matchesWhenPredicateReturnsTrue() {

    Precondition.param(new Object()).matches(Predicates.dummyTrue());
  }

  @Test(expected = InvalidParameterException.class)
  public void matchesWhenPredicateReturnsFalse() {

    Precondition.param(new Object()).matches(Predicates.dummyFalse());
  }
}
