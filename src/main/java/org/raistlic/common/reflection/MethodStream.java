package org.raistlic.common.reflection;

import org.raistlic.common.precondition.Precondition;
import org.raistlic.common.util.CustomStream;

import java.lang.reflect.Method;
import java.util.Collection;
import java.util.function.Predicate;
import java.util.stream.Stream;

/**
 * @author Lei Chen (2016-01-25)
 */
public final class MethodStream extends ExecutableStream<Method, MethodStream>
        implements CustomStream<Method, MethodStream> {

  public static MethodStream of(Stream<Method> methodStream) {

    Precondition.param(methodStream).isNotNull();
    return new MethodStream(methodStream);
  }

  public static MethodStream of(Collection<Method> methodCollection) {

    Precondition.param(methodCollection).isNotNull();
    return new MethodStream(methodCollection.stream());
  }

  MethodStream(Stream<Method> originalStream) {

    super(originalStream);
  }

  public MethodStream returnsVoid() {

    return hasReturnType(void.class);
  }

  public MethodStream hasReturnType(Class<?> returnType) {

    Precondition.param(returnType).isNotNull();
    return filter(m -> m.getReturnType() == returnType);
  }

  public MethodStream hasReturnTypeMatches(Predicate<? super Class<?>> returnTypePredicate) {

    Precondition.param(returnTypePredicate).isNotNull();
    return filter(m -> returnTypePredicate.test(m.getReturnType()));
  }

  public MethodStream hasName(String name) {

    return filter(ReflectionPredicates.methodHasName(name));
  }
}
