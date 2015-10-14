package org.raistlic.common.precondition;

import org.raistlic.common.util.Predicates;

import java.util.function.Predicate;

/**
 * @author Lei Chen (2015-10-14)
 */
final class ObjectPredicates {

  static final Predicate<Object> IS_NULL = new Predicate<Object>() {

    @Override
    public boolean test(Object o) {

      return o == null;
    }
  };

  static final Predicate<Object> NOT_NULL = new Predicate<Object>() {

    @Override
    public boolean test(Object o) {

      return o != null;
    }
  };

  static Predicate<Object> equalsObject(Object object) {

    return new EqualsPredicate(object);
  }

  static Predicate<Object> notEqualsObject(Object object) {

    return Predicates.not(equalsObject(object));
  }

  private static class EqualsPredicate implements Predicate<Object> {

    private Object object;

    private EqualsPredicate(Object object) {

      this.object = object;
    }

    @Override
    public boolean test(Object o) {

      if (object == null) {
        return o == null;
      }
      else {
        return object.equals(o);
      }
    }
  }
}
