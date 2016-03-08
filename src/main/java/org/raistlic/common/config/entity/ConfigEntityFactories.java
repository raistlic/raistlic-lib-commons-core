package org.raistlic.common.config.entity;

/**
 * The class is a static factory method holder for {@link ConfigEntityFactory} interface.
 *
 * @author Lei Chen (2016-03-08)
 */
public final class ConfigEntityFactories {

  /**
   * The method creates and returns an instance of the default implementation of the
   * {@link ConfigEntityFactory} interface.
   *
   * @return the newly created instance.
   */
  public static ConfigEntityFactory newConfigEntityFactory() {

    return new DefaultConfigEntityFactory();
  }

  /*
   * Not to be instantiated or inherited.
   */
  private ConfigEntityFactories() { }
}
