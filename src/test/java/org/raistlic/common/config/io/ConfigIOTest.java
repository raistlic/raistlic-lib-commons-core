package org.raistlic.common.config.io;

import junitparams.JUnitParamsRunner;
import junitparams.Parameters;
import junitparams.naming.TestCaseName;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.raistlic.common.config.core.Config;
import org.raistlic.common.config.core.ConfigBuilder;
import org.raistlic.common.config.core.ConfigFactory;
import org.raistlic.common.config.exception.ConfigIOException;
import org.raistlic.common.precondition.InvalidParameterException;
import org.raistlic.common.precondition.Precondition;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

import static org.fest.assertions.api.Assertions.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.spy;

/**
 * Unit test for the common contract of {@link ConfigIO} interface.
 *
 * @author Lei Chen (2015-10-11)
 */
@RunWith(JUnitParamsRunner.class)
@SuppressWarnings("unused")
public class ConfigIOTest {

  private static List<Object[]> failedCallCases() {

    List<Object[]> testCases = new ArrayList<Object[]>();
    testCases.add(new Object[]{

        ConfigIOFactory.getPropertiesConfigIO(), "ConfigIOFactory.getPropertiesConfigIO()"
    });
    testCases.add(new Object[]{

        ConfigIOFactory.getXmlConfigIO(), "ConfigIOFactory.getXmlConfigIO()"
    });
    testCases.add(new Object[]{

        ConfigIOFactory.getJsonConfigIO(), "ConfigIOFactory.getJsonConfigIO()"
    });
    testCases.add(new Object[]{

        ConfigIOFactory.getYamlConfigIO(), "ConfigIOFactory.getYamlConfigIO()"
    });
    return testCases;
  }

  @Test(expected = InvalidParameterException.class)
  @Parameters(method = "failedCallCases")
  @TestCaseName("writeConfigWithNullConfig with : {1}")
  public void writeConfigWithNullConfig(ConfigIO configIO, String description) {

    OutputStream outputStream = new ByteArrayOutputStream();
    configIO.writeConfig(null, outputStream);
  }

  @Test(expected = InvalidParameterException.class)
  @Parameters(method = "failedCallCases")
  @TestCaseName("writeConfigWithNullOutputStream with : {1}")
  public void writeConfigWithNullOutputStream(ConfigIO configIO, String description) {

    Config config = ConfigFactory.newMutableConfig();
    configIO.writeConfig(config, null);
  }

  @Test(expected = ConfigIOException.class)
  @Parameters(method = "failedCallCases")
  @TestCaseName("writeConfigWhenOutputStreamThrowsException with : {1}")
  public void writeConfigWhenOutputStreamThrowsException(ConfigIO configIO, String description)
      throws Exception {

    Config config = ConfigFactory.newMutableConfig()
        .setString("test.key", "test value")
        .get();
    OutputStream outputStream = spy(new ByteArrayOutputStream());
    doThrow(new RuntimeException("Test Exception")).when(outputStream).write(anyInt());
    doThrow(new RuntimeException("Test Exception")).when(outputStream).write(any(byte[].class));
    doThrow(new RuntimeException("Test Exception")).when(outputStream).write(any(byte[].class), anyInt(), anyInt());

    configIO.writeConfig(config, outputStream);
  }

  @Test(expected = InvalidParameterException.class)
  @Parameters(method = "failedCallCases")
  @TestCaseName("readConfigBuilderWithNullInputStream with : {1}")
  public void readConfigBuilderWithNullInputStream(ConfigIO configIO, String description) {

    ConfigBuilder configBuilder = ConfigFactory.newMutableConfig();
    configIO.readConfig(configBuilder, null);
  }

  @Test(expected = InvalidParameterException.class)
  @Parameters(method = "failedCallCases")
  @TestCaseName("readConfigBuilderWithNullConfigBuilder with : {1}")
  public void readConfigBuilderWithNullConfigBuilder(ConfigIO configIO, String description) {

    InputStream inputStream = new ByteArrayInputStream("test".getBytes());
    configIO.readConfig(null, inputStream);
  }

  @Test(expected = ConfigIOException.class)
  @Parameters(method = "failedCallCases")
  @TestCaseName("readConfigBuilderWhenInputStreamThrowsException with : {1}")
  public void readConfigBuilderWhenInputStreamThrowsException(ConfigIO configIO, String description)
      throws Exception {

    ConfigBuilder configBuilder = ConfigFactory.newMutableConfig();
    InputStream inputStream = spy(new ByteArrayInputStream("test".getBytes()));
    doThrow(new RuntimeException("Test Exception")).when(inputStream).read();
    doThrow(new RuntimeException("Test Exception")).when(inputStream).read(any(byte[].class));
    doThrow(new RuntimeException("Test Exception")).when(inputStream).read(any(byte[].class), anyInt(), anyInt());

    configIO.readConfig(configBuilder, inputStream);
  }

  @Test(expected = InvalidParameterException.class)
  @Parameters(method = "failedCallCases")
  @TestCaseName("readConfigWithNullInputStream with : {1}")
  public void readConfigWithNullInputStream(ConfigIO configIO, String description) {

    configIO.readConfig(null);
  }

  @Test(expected = ConfigIOException.class)
  @Parameters(method = "failedCallCases")
  @TestCaseName("readConfigWhenInputStreamThrowsException with : {1}")
  public void readConfigWhenInputStreamThrowsException(ConfigIO configIO, String description)
      throws Exception {

    InputStream inputStream = spy(new ByteArrayInputStream("test".getBytes()));
    doThrow(new RuntimeException("Test Exception")).when(inputStream).read();
    doThrow(new RuntimeException("Test Exception")).when(inputStream).read(any(byte[].class));
    doThrow(new RuntimeException("Test Exception")).when(inputStream).read(any(byte[].class), anyInt(), anyInt());

    configIO.readConfig(inputStream);
  }

  private static List<Object[]> expectedCases() {

    List<Object[]> testCases = new ArrayList<Object[]>();
    testCases.add(new Object[]{
        ConfigIOFactory.getPropertiesConfigIO(),
        "configuration.properties",
        "properties config io"
    });
    testCases.add(new Object[]{
        ConfigIOFactory.getXmlConfigIO(),
        "configuration.xml",
        "xml config io"
    });
    testCases.add(new Object[]{
        ConfigIOFactory.getJsonConfigIO(),
        "configuration.json",
        "json config io"
    });
    testCases.add(new Object[]{
        ConfigIOFactory.getYamlConfigIO(),
        "configuration.yml",
        "yaml config io"
    });
    return testCases;
  }

  @Test
  @Parameters(method = "expectedCases")
  @TestCaseName("writeConfigExpected with {2}")
  public void writeConfigExpected(ConfigIO configIO, String fixturePath, String description)
      throws Exception {

    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
    Config config = ConfigFixture.getConfigFixture();

    configIO.writeConfig(config, outputStream);

    String actual = new String(outputStream.toByteArray());
    InputStream inputStream = getClass().getResourceAsStream(fixturePath);
    String expected = readAll(inputStream);
    inputStream.close();
    assertThat(actual).isEqualTo(expected);
  }

  @Test
  @Parameters(method = "expectedCases")
  @TestCaseName("readConfigExpected with {2}")
  public void readConfigExpected(ConfigIO configIO, String fixturePath, String description)
      throws Exception {

    InputStream inputStream = getClass().getResourceAsStream(fixturePath);
    Config actual = configIO.readConfig(inputStream);
    inputStream.close();
    Config expected = ConfigFixture.getConfigFixture();

    assertThat(actual.getKeys()).hasSize(expected.getKeys().size());
    assertThat(actual.getKeys()).containsOnly(expected.getKeys().toArray(new String[expected.getKeys().size()]));
    for (String key : expected.getKeys()) {
      assertThat(actual.getString(key)).isEqualTo(expected.getString(key));
    }
  }

  private static String readAll(InputStream inputStream) throws IOException {

    Precondition.param(inputStream, "inputStream").isNotNull();

    ByteArrayOutputStream bytes = new ByteArrayOutputStream();
    int read;
    do {
      read = inputStream.read(BUFFER);
      if (read > 0) {
        bytes.write(BUFFER, 0, read);
      }
      read = inputStream.read(BUFFER);
    } while (read == BUFFER_SIZE);
    return new String(bytes.toByteArray());
  }

  private static final int BUFFER_SIZE = 4096;

  private static final byte[] BUFFER = new byte[BUFFER_SIZE];
}
