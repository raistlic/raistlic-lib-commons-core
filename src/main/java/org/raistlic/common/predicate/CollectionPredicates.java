package org.raistlic.common.predicate;

import org.raistlic.common.precondition.Precondition;

import java.util.Collection;
import java.util.function.Predicate;

/**
 * Static factory methods holder that exposes different kinds of {@link Predicate} instances for
 * testing {@link Collection} .
 */
public final class CollectionPredicates {

  /**
   * Returns a {@link Predicate} instance to test whether a {@link Collection} is empty. A {@link Collection} is considered
   * empty when it is not {@code null} and has size of {@code 0} .
   *
   * @return the {@link Predicate} instance.
   */
  public static Predicate<Collection<?>> isEmpty() {

    return CollectionIsEmptyPredicate.INSTANCE;
  }

  /**
   * Returns a {@link Predicate} instance to test whether a {@link Collection} is not empty. A {@link Collection} is
   * considered not empty when it is {@code null} or its size is greater than {@code 0} .
   *
   * @return the {@link Predicate} instance.
   */
  public static Predicate<Collection<?>> notEmpty() {

    return COLLECTION_NOT_EMPTY_INSTANCE;
  }

  /**
   * Returns a {@link Predicate} instance to test a {@link Collection} is with the specified {@code size} .
   *
   * @param size the size to test, cannot be less than {@code 0} .
   * @return the {@link Predicate} instance.
   */
  public static Predicate<Collection<?>> hasSize(int size) {

    Precondition.assertParam(size >= 0, "hasSize('size') size must be no less than 0, but was " + size);
    return new CollectionWithSizePredicate(size);
  }

  /**
   * Returns a {@link Predicate} instance to test a {@link Collection} containing the specified {@code element} .
   *
   * @param element the element to test
   * @param <E>     the concrete element type of the collection
   * @return the {@link Predicate} instance.
   */
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

  private static final Predicate<Collection<?>> COLLECTION_NOT_EMPTY_INSTANCE =
    Predicates.not(CollectionIsEmptyPredicate.INSTANCE);

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
  private CollectionPredicates() {
  }
}
