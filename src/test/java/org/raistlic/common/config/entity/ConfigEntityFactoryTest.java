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

package org.raistlic.common.config.entity;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import org.raistlic.common.config.core.Config;
import org.raistlic.common.config.core.ConfigFactory;

import static org.raistlic.common.postcondition.Postcondition.assertThat;

/**
 * @author Lei Chen (2015-12-29)
 */
@RunWith(JUnit4.class)
public class ConfigEntityFactoryTest {

  private Config config;

  private ConfigEntityFactory configEntityFactory;

  @Before
  public void setup() {

    config = ConfigFactory.newMutableConfig()
            .setInt("test.config.myInt", 123)
            .setString("test.config.myString", "8d5bc826-9b5f-4bac-b461-0b048386a659")
            .setBoolean("test.config.myFlag", true)
            .setLong("test.config.myLong", -1L)
            .get();
    configEntityFactory = ConfigEntityFactories.newConfigEntityFactory();
  }

  @Test
  public void createByFactoryMethodExpected() {

    CreateByFactoryMethod entity = configEntityFactory.createConfigEntity(
            CreateByFactoryMethod.class, config, "test.config");
    assertThat(entity).isNotNull();
    assertThat(entity.getMyInt()).isEqualTo(123);
    assertThat(entity.getMyString()).isEqualTo("8d5bc826-9b5f-4bac-b461-0b048386a659");
    assertThat(entity.isMyFlag()).isTrue();
    assertThat(entity.getMyLong()).isEqualTo(-1L);
  }

  @Test
  public void createByConstructorExpected() {

    CreateByConstructor entity = configEntityFactory.createConfigEntity(
            CreateByConstructor.class, config, "test");
    assertThat(entity).isNotNull();
    assertThat(entity.getMyInt()).isEqualTo(123);
    assertThat(entity.getMyString()).isEqualTo("8d5bc826-9b5f-4bac-b461-0b048386a659");
    assertThat(entity.isMyFlag()).isTrue();
    assertThat(entity.getMyLong()).isEqualTo(-1L);
  }

  @Test
  public void createNestedExpected() {

    MyNested entity = configEntityFactory.createConfigEntity(MyNested.class, config, "test");

    assertThat(entity).isNotNull();
    MyConfig config = entity.getMyConfig();
    assertThat(config).isNotNull();
    assertThat(config.getMyInt()).isEqualTo(123);
    assertThat(config.getMyString()).isEqualTo("8d5bc826-9b5f-4bac-b461-0b048386a659");
    assertThat(config.isMyFlag()).isTrue();
    assertThat(config.getMyLong()).isEqualTo(-1L);
  }

  public static class CreateByFactoryMethod {

    @ConfigConstructor
    public static CreateByFactoryMethod factoryMethod(@ConfigProperty("myInt") int myInt,
                                                      @ConfigProperty("myString") String myString,
                                                      @ConfigProperty("myFlag") boolean myFlag) {

      return new CreateByFactoryMethod(myInt, myString, myFlag);
    }

    private final int myInt;

    private final String myString;

    private final boolean myFlag;

    @ConfigProperty
    private long myLong;

    private CreateByFactoryMethod(int myInt, String myString, boolean myFlag) {

      this.myInt = myInt;
      this.myString = myString;
      this.myFlag = myFlag;
    }

    public int getMyInt() {

      return myInt;
    }

    public String getMyString() {

      return myString;
    }

    public boolean isMyFlag() {

      return myFlag;
    }

    public long getMyLong() {

      return myLong;
    }
  }

  public static class CreateByConstructor {

    private final int myInt;

    private final String myString;

    private final boolean myFlag;

    @ConfigProperty("config.myLong")
    private long myLong;

    @ConfigConstructor
    public CreateByConstructor(@ConfigProperty("config.myInt") int myInt,
                               @ConfigProperty("config.myString") String myString,
                               @ConfigProperty("config.myFlag") boolean myFlag) {

      this.myInt = myInt;
      this.myString = myString;
      this.myFlag = myFlag;
    }

    public int getMyInt() {

      return myInt;
    }

    public String getMyString() {

      return myString;
    }

    public boolean isMyFlag() {

      return myFlag;
    }

    public long getMyLong() {

      return myLong;
    }
  }

  public static class MyNested {

    private MyConfig myConfig;

    public MyNested(@ConfigProperty("config") MyConfig myConfig) {

      this.myConfig = myConfig;
    }

    public MyConfig getMyConfig() {

      return myConfig;
    }
  }

  public static class MyConfig {

    @ConfigProperty
    private String myString;

    @ConfigProperty
    private int myInt;

    @ConfigProperty
    private boolean myFlag;

    @ConfigProperty
    private long myLong;

    public String getMyString() {

      return myString;
    }

    public int getMyInt() {

      return myInt;
    }

    public boolean isMyFlag() {

      return myFlag;
    }

    public long getMyLong() {

      return myLong;
    }
  }
}
