package org.raistlic.common.reflection;

import org.raistlic.common.util.CustomStream;
import org.raistlic.common.util.CustomStreamAdapter;

import java.lang.reflect.Member;
import java.util.stream.Stream;

/**
 * @author Lei Chen (2016-03-11)
 */
public abstract class MemberStream<M extends Member, MS extends MemberStream<M, MS>>
        extends CustomStreamAdapter<M, MS> implements CustomStream<M, MS> {

  MemberStream(Stream<M> originalStream) {

    super(originalStream);
  }

  public MS staticOnes() {

    return filter(ReflectionPredicates.memberIsStatic());
  }

  public MS noneStaticOnes() {

    return filter(ReflectionPredicates.memberIsNotStatic());
  }

  public MS publicOnes() {

    return filter(ReflectionPredicates.memberIsPublic());
  }

  public MS protectedOnes() {

    return filter(ReflectionPredicates.memberIsProtected());
  }

  public MS publicAndProtectedOnes() {

    return filter(ReflectionPredicates.memberIsPublicOrProtected());
  }

  public MS privateOnes() {

    return filter(ReflectionPredicates.memberIsPrivate());
  }

  public MS packagePrivateOnes() {

    return filter(ReflectionPredicates.memberIsPackagePrivate());
  }

  public MS privateOrPackagePrivateOnes() {

    return filter(ReflectionPredicates.memberIsPrivateOrPackagePrivate());
  }

  public MS finalOnes() {

    return filter(ReflectionPredicates.memberIsFinal());
  }

  public MS abstractOnes() {

    return filter(ReflectionPredicates.memberIsAbstract());
  }

  public MS nativeOnes() {

    return filter(ReflectionPredicates.memberIsNative());
  }
}
