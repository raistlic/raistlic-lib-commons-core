package org.raistlic.common.codec;

import junitparams.JUnitParamsRunner;
import junitparams.Parameters;
import junitparams.naming.TestCaseName;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.raistlic.common.precondition.InvalidParameterException;

import java.util.ArrayList;
import java.util.List;

import static org.raistlic.common.postcondition.Postcondition.assertThat;

/**
 * @author Lei Chen (2015-12-25)
 */
@RunWith(JUnitParamsRunner.class)
public class DeserializersTest {

  @Test
  public void getStringDeserializerReturnsSingleton() {

    Deserializer<String> deserializer = Deserializers.getStringDeserializer();

    assertThat(Deserializers.getStringDeserializer() == deserializer).isTrue();
    assertThat(Deserializers.getStringDeserializer() == deserializer).isTrue();
    assertThat(Deserializers.getStringDeserializer() == deserializer).isTrue();
  }

  @Test(expected = InvalidParameterException.class)
  public void getStringDeserializerDoesNotAcceptNullTarget() {

    Deserializer<String> deserializer = Deserializers.getStringDeserializer();
    deserializer.decode(null);
  }

  @Test
  public void getStringDeserializerDecodeExpected() {

    String target = "cdbebc05-8409-49ee-9632-c3143755ca6a";

    Deserializer<String> deserializer = Deserializers.getStringDeserializer();
    assertThat(deserializer).isNotNull();

    String actual = deserializer.decode(target);
    assertThat(actual).isEqualTo(target);
  }

  @Test
  public void getBooleanDeserializerReturnsSingleton() {

    Deserializer<Boolean> deserializer = Deserializers.getBooleanDeserializer();

    assertThat(Deserializers.getBooleanDeserializer() == deserializer).isTrue();
    assertThat(Deserializers.getBooleanDeserializer() == deserializer).isTrue();
    assertThat(Deserializers.getBooleanDeserializer() == deserializer).isTrue();
  }

  @Test(expected = InvalidParameterException.class)
  public void getBooleanDeserializerDoesNotAcceptNullTarget() {

    Deserializer<Boolean> deserializer = Deserializers.getBooleanDeserializer();
    deserializer.decode(null);
  }

  @Test(expected = ValueConversionException.class)
  @Parameters(method = "invalidBooleanTargetCases")
  @TestCaseName("getBooleanDeserializerThrowsExceptionOnInvalidTargets : {0}")
  public void getBooleanDeserializerThrowsExceptionOnInvalidTargets(String invalidTarget) {

    Deserializer<Boolean> deserializer = Deserializers.getBooleanDeserializer();
    deserializer.decode(invalidTarget);
  }

  @SuppressWarnings("unused")
  private static List<Object[]> invalidBooleanTargetCases() {

    List<Object[]> cases = new ArrayList<>();
    cases.add(new Object[]{""});
    cases.add(new Object[]{"123"});
    cases.add(new Object[]{"abc"});
    cases.add(new Object[]{"1true"});
    cases.add(new Object[]{"false0"});
    cases.add(new Object[]{"tru"});
    cases.add(new Object[]{"fal se"});
    return cases;
  }

  @Test
  @TestCaseName("getBooleanDeserializerDecodeExpected with '{0}', expect: {1}")
  @Parameters(method = "validBooleanTargetCases")
  public void getBooleanDeserializerDecodeExpected(String target, Boolean expected) {

    Deserializer<Boolean> deserializer = Deserializers.getBooleanDeserializer();
    assertThat(deserializer.decode(target)).isEqualTo(expected);
  }

  @SuppressWarnings("unused")
  private static List<Object[]> validBooleanTargetCases() {

    List<Object[]> cases = new ArrayList<>();
    cases.add(new Object[]{"true", true});
    cases.add(new Object[]{"True", true});
    cases.add(new Object[]{"TRUE", true});
    cases.add(new Object[]{" true", true});
    cases.add(new Object[]{"true ", true});
    cases.add(new Object[]{" TRUE ", true});
    cases.add(new Object[]{"false", false});
    cases.add(new Object[]{"False", false});
    cases.add(new Object[]{"FALSE", false});
    cases.add(new Object[]{"false ", false});
    cases.add(new Object[]{" false", false});
    cases.add(new Object[]{" FALSE ", false});
    return cases;
  }

  @Test
  public void getByteDeserializerReturnsSingleton() {

    Deserializer<Byte> deserializer = Deserializers.getByteDeserializer();

    assertThat(Deserializers.getByteDeserializer() == deserializer).isTrue();
    assertThat(Deserializers.getByteDeserializer() == deserializer).isTrue();
    assertThat(Deserializers.getByteDeserializer() == deserializer).isTrue();
  }

  @Test(expected = InvalidParameterException.class)
  public void getByteDeserializerDoesNotAcceptNullTarget() {

    Deserializer<Byte> deserializer = Deserializers.getByteDeserializer();
    deserializer.decode(null);
  }

  @Test(expected = ValueConversionException.class)
  @Parameters(method = "invalidByteTargetCases")
  @TestCaseName("getByteDeserializerThrowsExceptionOnInvalidTargets : {0}")
  public void getByteDeserializerThrowsExceptionOnInvalidTargets(String invalidTargets) {

    Deserializer<Byte> deserializer = Deserializers.getByteDeserializer();
    deserializer.decode(invalidTargets);
  }

  @SuppressWarnings("unused")
  private static List<Object[]> invalidByteTargetCases() {

    List<Object[]> cases = new ArrayList<>();
    cases.add(new Object[]{""});
    cases.add(new Object[]{"some text"});
    cases.add(new Object[]{"236482346234"});
    cases.add(new Object[]{"@#$Sdf"});
    return cases;
  }

  @Test
  @Parameters(method = "validByteTargetCases")
  @TestCaseName("getByteDeserializerDecodeExpected with {0}, expect: {1}")
  public void getByteDeserializerDecodeExpected(String target, Byte expected) {

    Deserializer<Byte> deserializer = Deserializers.getByteDeserializer();
    assertThat(deserializer.decode(target)).isEqualTo(expected);
  }

  @SuppressWarnings("unused")
  private static List<Object[]> validByteTargetCases() {

    List<Object[]> cases = new ArrayList<>();
    cases.add(new Object[]{"123", (byte)123});
    cases.add(new Object[]{"-123", (byte)-123});
    cases.add(new Object[]{"0", (byte)0});
    cases.add(new Object[]{"1", (byte)1});
    cases.add(new Object[]{"-1", (byte)-1});
    return cases;
  }
}
