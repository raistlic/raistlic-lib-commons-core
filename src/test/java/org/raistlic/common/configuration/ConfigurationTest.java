package org.raistlic.common.configuration;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.raistlic.common.precondition.InvalidParameterException;

import java.util.Arrays;
import java.util.Collection;

import static org.fest.assertions.Assertions.assertThat;

/**
 * Unit test for {@link Configuration} interface contract.
 *
 * @author Lei.C (2015-05-01)
 */
@RunWith(Parameterized.class)
public class ConfigurationTest {

  @Parameterized.Parameters
  public static Collection<Object[]> getTestParameters() {

    return Arrays.<Object[]>asList(
            new Object[] { new DefaultMutableConfiguration() }
    );
  }

  private Configuration configuration;

  public ConfigurationTest(Configuration.Builder builder) {

    populateConfigurationBuilderWithFixtureValues(builder);
    configuration = builder.build();
  }

  /**
   * Getting String value with configured key should return the configured value.
   */
  @Test
  public void testGetString() {

    assertThat(configuration.getString("string.value")).isEqualTo(FIXTURE_STRING);
  }

  /**
   * Getting String with null key should cause a {@link org.raistlic.common.precondition.InvalidParameterException} .
   */
  @Test(expected = InvalidParameterException.class)
  public void testGetStringWithNullKey() {

    configuration.getString(null);
  }

  /**
   * Getting String with not configured key should return null.
   */
  @Test
  public void testGetStringNotConfigured() {

    String actual = configuration.getString("not.configured");
    assertThat(actual).isNull();
  }

  /**
   * Getting String with not configured key and a default value should return the default value.
   */
  @Test
  public void testGetStringNotConfiguredWithDefaultValue() {

    String expected = "expectedDefaultValue";
    String actual = configuration.getString("not.configured", expected);
    assertThat(actual).isEqualTo(expected);
  }

  /**
   * Getting boolean with correct key should return the configured value.
   */
  @Test
  public void testGetBoolean() {

    assertThat(configuration.getBoolean("boolean.value", false)).isTrue();
  }

  /**
   * Getting boolean value with null key should cause {@link InvalidParameterException} .
   */
  @Test(expected = InvalidParameterException.class)
  public void testGetBooleanWithNullKey() {

    configuration.getBoolean(null, true);
  }

  /**
   * Getting boolean with not configured key should return the provided default value.
   */
  @Test
  public void testGetBooleanNotConfigured() {

    boolean expected = false;
    boolean actual = configuration.getBoolean("not.configured", expected);
    assertThat(actual).isEqualTo(expected);
  }

  /**
   * Getting byte value with the correct key should return the configured value.
   */
  @Test
  public void testGetByte() {

    assertThat(configuration.getByte("byte.value", (byte) 1)).isEqualTo(FIXTURE_BYTE);
  }

  /**
   * Getting byte with not configured key should return the provided default value.
   */
  @Test
  public void testGetByteNotConfigured() {

    byte expected = (byte) 44;
    byte actual = configuration.getByte("not.configured", expected);
    assertThat(actual).isEqualTo(expected);
  }

  private static void populateConfigurationBuilderWithFixtureValues(Configuration.Builder builder) {

    builder.setByte("byte.value", FIXTURE_BYTE);
    builder.setChar("char.value", FIXTURE_CHAR);
    builder.setInt("int.value", FIXTURE_INT);
    builder.setShort("short.value", FIXTURE_SHORT);
    builder.setLong("long.value", FIXTURE_LONG);
    builder.setFloat("float.value", FIXTURE_FLOAT);
    builder.setDouble("double.value", FIXTURE_DOUBLE);
    builder.setBoolean("boolean.value", FIXTURE_BOOLEAN);
    builder.setString("string.value", FIXTURE_STRING);
  }

  private static final byte FIXTURE_BYTE = (byte) 12;

  private static final char FIXTURE_CHAR = 'c';

  private static final short FIXTURE_SHORT = -325;

  private static final int FIXTURE_INT = 12345;

  private static final long FIXTURE_LONG = -234523458923234L;

  private static final float FIXTURE_FLOAT = 123.0F;

  private static final double FIXTURE_DOUBLE = 234.1;

  private static final boolean FIXTURE_BOOLEAN = true;

  private static final String FIXTURE_STRING = "8c41ad9c-efb7-11e4-90ec-1681e6b88ec1";
}
