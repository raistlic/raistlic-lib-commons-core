package org.raistlic.common.reflection;

import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import org.raistlic.common.reflection.fixtures.AnotherClass;
import org.raistlic.common.reflection.fixtures.TestClass;
import org.raistlic.common.reflection.fixtures.TestInterface;

import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.Arrays;
import java.util.function.Predicate;

import static org.fest.assertions.api.Assertions.assertThat;

/**
 * @author Lei Chen (2016-03-12)
 */
@RunWith(JUnit4.class)
@Ignore("the reflection utils is a mess atm.")
public class MethodPredicatesTest {

  @Test
  public void smokeTest3() throws Exception {

    Method method = TestClass.class.getMethod("testMethodWithGenericParam", Integer.class, String.class);
    Parameter[] params = method.getParameters();
    System.out.println(params[0]);
  }

  @Test
  public void smokeTest() throws Exception {

    Method method = TestInterface.class.getMethod("testMethodWithGenericParam", Number.class, String.class);
    Parameter parameter = method.getParameters()[0];
    Type parameterizedType = parameter.getParameterizedType();
    System.out.println(parameterizedType);
    System.out.println(parameter);

    Parameter parameter2 = method.getParameters()[1];
    Type type2 = parameter2.getParameterizedType();
    System.out.println(type2);
  }

  @Test
  public void smokeTest2() throws Exception {

    System.out.println(Arrays.asList(TestClass.class.getGenericInterfaces()));
    System.out.println(Arrays.asList(TestClass.class.getInterfaces()));

    System.out.println(TestClass.class.getGenericInterfaces()[0] == TestClass.class.getInterfaces()[0]);

    Object o = TestClass.class.getGenericInterfaces()[0];
    if (o instanceof ParameterizedType) {
      ParameterizedType pt = (ParameterizedType) o;
      System.out.println(pt.getRawType());
      System.out.println(pt.getOwnerType());
      System.out.println(Arrays.asList(pt.getActualTypeArguments()));

      System.out.println("asdf: " + (pt.getRawType() == TestClass.class.getInterfaces()[0]));
    }

    TypeVariable<Class<TestInterface>>[] typeParameters = TestInterface.class.getTypeParameters();
    for (TypeVariable<Class<TestInterface>> var : typeParameters) {
      System.out.println(var);
    }

    TypeVariable<Class<TestInterface>> first = typeParameters[0];
  }

  @Test
  public void methodOverridesExpected() throws Exception {

    Method methodOverridden = TestInterface.class.getMethod("testVoidMethod");
    Method method = TestClass.class.getMethod("testVoidMethod");
    Method methodInAnotherClass = AnotherClass.class.getMethod("testVoidMethod");
    Predicate<Method> predicate = ReflectionPredicates.methodOverrides(methodOverridden);

    assertThat(predicate.test(method)).isTrue();
    assertThat(predicate.test(methodOverridden)).isFalse();
    assertThat(predicate.test(methodInAnotherClass)).isFalse();
  }

  @Test
  public void methodOverridesGenericReturnMethodExpected() throws Exception {

    Method methodOverridden = TestInterface.class.getMethod("testGenericReturnMethod");
    Method method = TestClass.class.getMethod("testGenericReturnMethod");
    Predicate<Method> predicate = ReflectionPredicates.methodOverrides(methodOverridden);

    assertThat(predicate.test(method)).isTrue();
  }

  @Test
  @Ignore("TODO get back to this in the future... maybe")
  public void methodOverridesGenericParamMethodExpected() throws Exception {

    Method methodOverridden = TestInterface.class.getMethod("testMethodWithGenericParam", Object.class);
    Method method = TestClass.class.getMethod("testMethodWithGenericParam", String.class);
    Predicate<Method> predicate = ReflectionPredicates.methodOverrides(methodOverridden);

    assertThat(predicate.test(method)).isTrue();
  }
}
