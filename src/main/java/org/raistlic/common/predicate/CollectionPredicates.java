package org.raistlic.common.predicate;

import org.raistlic.common.precondition.Precondition;

import java.util.Collection;
import java.util.function.Predicate;

/**
 * Static factory methods holder that exposes different kinds of {@link Predicate} instances for
 * testing {@link Collection} .
 *
 * @author Lei Chen (2016-03-17)
 */
public final class CollectionPredicates {

  public static Predicate<Collection<?>> isEmpty() {

    return CollectionIsEmptyPredicate.INSTANCE;
  }

  public static Predicate<Collection<?>> notEmpty() {

    return Predicates.not(isEmpty());
  }

  public static Predicate<Collection<?>> hasSize(int size) {

    Precondition.assertParam(size >= 0, "hasSize('size') size must be no less than 0, but was " + size);
    return new CollectionWithSizePredicate(size);
  }

  public static <E> Predicate<Collection<E>> contains(E element) {

    return new CollectionContainsPredicate<>(element);
  }

  private enum CollectionIsEmptyPredicate implements Predicate<Collection<?>> {

    INSTANCE;

    @Override
    public boolean test(Collection<?> collection) {

      return collection != null && collection.isEmpty();
    }
  }

  private static class CollectionWithSizePredicate implements Predicate<Collection<?>> {

    private final int size;

    private CollectionWithSizePredicate(int size) {

      this.size = size;
    }

    @Override
    public boolean test(Collection<?> objects) {

      return objects != null && objects.size() == size;
    }
  }

  private static class CollectionContainsPredicate<E> implements Predicate<Collection<E>> {

    private final E element;

    private CollectionContainsPredicate(E element) {

      this.element = element;
    }

    @Override
    public boolean test(Collection<E> es) {

      return es != null && es.contains(element);
    }
  }

  /*
   * Not to be instantiated or inherited.
   */
  private CollectionPredicates() { }
}
