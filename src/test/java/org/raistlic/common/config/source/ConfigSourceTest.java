/*
 * Copyright 2015 Lei CHEN (raistlic@gmail.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.raistlic.common.config.source;

import junitparams.JUnitParamsRunner;
import junitparams.Parameters;
import junitparams.naming.TestCaseName;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.raistlic.common.precondition.InvalidParameterException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import static org.fest.assertions.api.Assertions.assertThat;
import static org.junit.Assert.fail;

/**
 * The unit test for common contract of the {@link ConfigSource} interface.
 *
 * @author Lei Chen (2015-09-11)
 */
@RunWith(JUnitParamsRunner.class)
@SuppressWarnings("unused")
public class ConfigSourceTest {

  private static final Map<String, String> SAMPLE_VALUES_MAP;

  private static final Properties SAMPLE_VALUES_PROPERTIES;

  private static List<Object[]> getTestCases() {

    List<Object[]> testCases = new ArrayList<Object[]>();
    ConfigSource instance;

    // empty immutable instance
    instance = ConfigSourceFactory.immutableEmptySource();
    testCases.add(new Object[]{
        instance,
        instance.getClass(),
        "empty instance"
    });

    // immutable instance with sample map values
    instance = ConfigSourceFactory.immutableCopyOf(SAMPLE_VALUES_MAP);
    testCases.add(new Object[]{
        instance,
        instance.getClass(),
        "immutable instance with sample map values"
    });

    // immutable instance with sample properties values
    instance = ConfigSourceFactory.immutableCopyOf(SAMPLE_VALUES_PROPERTIES);
    testCases.add(new Object[]{
        instance,
        instance.getClass(),
        "immutable instance with sample properties values"
    });

    // wrapper instance with sample map values
    instance = ConfigSourceFactory.wrap(SAMPLE_VALUES_MAP);
    testCases.add(new Object[]{
        instance,
        instance.getClass(),
        "wrapper instance with sample map values"
    });

    // wrapper instance with sample properties values
    instance = ConfigSourceFactory.wrap(SAMPLE_VALUES_PROPERTIES);
    testCases.add(new Object[]{
        instance,
        instance.getClass(),
        "wrapper instance with sample properties values"
    });

    return testCases;
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("'getKeysNotNull' for implementation type: {1}, {2}")
  public void getKeysNotNull(ConfigSource configSource,
                             Class<? extends ConfigSource> configSourceType,
                             String instanceDescription) {

    Set<String> keys = configSource.getKeys();
    assertThat(keys).isNotNull();
  }

  @Test
  @Parameters(method = "getTestCases")
  @TestCaseName("'getKeysReadOnly' for implementation type: {1}, {2}")
  public void getKeysReadOnly(ConfigSource configSource,
                              Class<? extends ConfigSource> configSourceType,
                              String instanceDescription) {

    Set<String> keys = configSource.getKeys();
    try {
      keys.add("abc");
      fail("Expected exception not thrown: " + UnsupportedOperationException.class);
    }
    catch (UnsupportedOperationException ex) {
      // do nothing
    }
  }

  @Test(expected = InvalidParameterException.class)
  @Parameters(method = "getTestCases")
  @TestCaseName("'getStringWithNullKey' for implementation type: {1}, {2}")
  public void getStringWithNullKey(ConfigSource configSource,
                                   Class<? extends ConfigSource> configSourceType,
                                   String instanceDescription) {

    configSource.getString(null);
  }

  static {

    SAMPLE_VALUES_PROPERTIES = new Properties();
    SAMPLE_VALUES_PROPERTIES.put("testKey1", "testValue1");
    SAMPLE_VALUES_PROPERTIES.put("testKey2", "testValue2");
    SAMPLE_VALUES_PROPERTIES.put("testKey3", "testValue3");
    SAMPLE_VALUES_PROPERTIES.put("testKey4", "testValue4");
    SAMPLE_VALUES_PROPERTIES.put("testKey5", "testValue5");

    SAMPLE_VALUES_MAP = new HashMap<String, String>();
    for (Object key : SAMPLE_VALUES_PROPERTIES.keySet()) {
      SAMPLE_VALUES_MAP.put(String.valueOf(key), String.valueOf(SAMPLE_VALUES_PROPERTIES.get(key)));
    }
  }
}
