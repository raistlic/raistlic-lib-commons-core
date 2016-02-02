package org.raistlic.common.config.entity;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import org.raistlic.common.config.core.Config;
import org.raistlic.common.config.core.ConfigFactory;

import static org.fest.assertions.api.Assertions.assertThat;

/**
 * @author Lei Chen (2015-12-29)
 */
@RunWith(JUnit4.class)
public class ConfigEntityFactoryTest {

  private Config config;

  @Before
  public void prepareConfig() {

    config = ConfigFactory.newMutableConfig()
            .setInt("test.config.myInt", 123)
            .setString("test.config.myString", "8d5bc826-9b5f-4bac-b461-0b048386a659")
            .setBoolean("test.config.myFlag", true)
            .setLong("test.config.myLong", -1L)
            .get();
  }

  @Test
  public void createByFactoryMethodExpected() {

    ConfigEntityFactory configEntityFactory = new ConfigEntityFactory();

    CreateByFactoryMethod entity = configEntityFactory.createConfigEntity(
            config, CreateByFactoryMethod.class);
    assertThat(entity).isNotNull();
    assertThat(entity.getMyInt()).isEqualTo(123);
    assertThat(entity.getMyString()).isEqualTo("8d5bc826-9b5f-4bac-b461-0b048386a659");
    assertThat(entity.isMyFlag()).isTrue();
    assertThat(entity.getMyLong()).isEqualTo(-1L);
  }

  @Test
  public void createByConstructorExpected() {

    ConfigEntityFactory configEntityFactory = new ConfigEntityFactory();

    CreateByConstructor entity = configEntityFactory.createConfigEntity(
            config, CreateByConstructor.class);
    assertThat(entity).isNotNull();
    assertThat(entity.getMyInt()).isEqualTo(123);
    assertThat(entity.getMyString()).isEqualTo("8d5bc826-9b5f-4bac-b461-0b048386a659");
    assertThat(entity.isMyFlag()).isTrue();
    assertThat(entity.getMyLong()).isEqualTo(-1L);
  }

  @ConfigEntity(name = "beanName", path = "test.config")
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

  @ConfigEntity(name = "beanName", path = "test")
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
}
