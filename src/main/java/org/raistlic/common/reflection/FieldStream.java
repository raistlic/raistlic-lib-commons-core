package org.raistlic.common.reflection;

import org.raistlic.common.precondition.Precondition;
import org.raistlic.common.util.CustomStream;
import org.raistlic.common.util.CustomStreamAdapter;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.function.Predicate;
import java.util.stream.Stream;

/**
 * @author Lei Chen (2016-01-25)
 */
public final class FieldStream extends CustomStreamAdapter<Field, FieldStream>
        implements CustomStream<Field, FieldStream> {

  FieldStream(Stream<Field> originalStream) {

    super(originalStream);
  }

  public FieldStream nameEquals(String expectedName) {

    Precondition.param(expectedName, "expectedName").notNull();
    return filter(f -> f.getName().equals(expectedName));
  }

  public FieldStream nameMatches(Predicate<String> namePredicate) {

    Precondition.param(namePredicate, "namePredicate").notNull();
    return filter(f -> namePredicate.test(f.getName()));
  }

  public FieldStream ofType(Class<?> expectedType) {

    Precondition.param(expectedType, "expectedType").notNull();
    return filter(f -> f.getType() == expectedType);
  }

  public FieldStream staticOnes() {

    return filter(PREDICATE_IS_STATIC);
  }

  public FieldStream noneStaticOnes() {

    return filter(PREDICATE_IS_NOT_STATIC);
  }

  public FieldStream annotatedWith(Class<? extends Annotation> annotationType) {

    Precondition.param(annotationType, "annotationType").notNull();
    return filter(ReflectionPredicates.elementAnnotatedWith(annotationType));
  }

  private static final Predicate<Field> PREDICATE_IS_STATIC = f -> Modifier.isStatic(f.getModifiers());

  private static final Predicate<Field> PREDICATE_IS_NOT_STATIC = f -> !Modifier.isStatic(f.getModifiers());
}
