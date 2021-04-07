package org.raistlic.common.reflection;

import org.raistlic.common.precondition.Precondition;
import org.raistlic.common.util.CustomStream;
import org.raistlic.common.util.CustomStreamAdapter;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Collection;
import java.util.function.Predicate;
import java.util.stream.Stream;

/**
 * @deprecated will be removed in 2.0, this one went too far.
 */
@Deprecated
public final class FieldStream extends CustomStreamAdapter<Field, FieldStream>
        implements CustomStream<Field, FieldStream> {

  public static FieldStream of(Stream<Field> fieldStream) {

    Precondition.param(fieldStream).isNotNull();
    return new FieldStream(fieldStream);
  }

  public static FieldStream of(Collection<Field> fieldCollection) {

    Precondition.param(fieldCollection).isNotNull();
    return new FieldStream(fieldCollection.stream());
  }

  FieldStream(Stream<Field> originalStream) {

    super(originalStream);
  }

  public FieldStream nameEquals(String expectedName) {

    Precondition.param(expectedName).isNotNull();
    return filter(f -> f.getName().equals(expectedName));
  }

  public FieldStream nameMatches(Predicate<String> namePredicate) {

    Precondition.param(namePredicate).isNotNull();
    return filter(f -> namePredicate.test(f.getName()));
  }

  public FieldStream ofType(Class<?> expectedType) {

    Precondition.param(expectedType).isNotNull();
    return filter(f -> f.getType() == expectedType);
  }

  public FieldStream staticOnes() {

    return filter(PREDICATE_IS_STATIC);
  }

  public FieldStream noneStaticOnes() {

    return filter(PREDICATE_IS_NOT_STATIC);
  }

  public FieldStream annotatedWith(Class<? extends Annotation> annotationType) {

    Precondition.param(annotationType).isNotNull();
    return filter(ReflectionPredicates.elementAnnotatedWith(annotationType));
  }

  private static final Predicate<Field> PREDICATE_IS_STATIC = f -> Modifier.isStatic(f.getModifiers());

  private static final Predicate<Field> PREDICATE_IS_NOT_STATIC = f -> !Modifier.isStatic(f.getModifiers());
}
