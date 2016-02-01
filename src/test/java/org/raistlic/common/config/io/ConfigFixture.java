package org.raistlic.common.config.io;

import org.raistlic.common.config.core.Config;
import org.raistlic.common.config.core.ConfigFactory;

/**
 * @author Lei Chen (2015-10-12)
 */
final class ConfigFixture {

  private static final Config CONFIG_FIXTURE = ConfigFactory.newMutableConfig()
      .setString("1d6737b9.c307.4fd9.a82f.77c8485a1f43", "60cd8178.ce9a.4fd0.aad5.fe2168046f44")
      .setString("1d6737b9.8668.43b7.9e95.63c8c0662424", "4d318577.33c3.425d.a73c.5dd1efd0ce06")
      .setString("cc3cfa2a.ce0d.4830.b8a5.31a748be8a72", "07c7ae9d.9c42.4ef7.84b3.05f102ba70e2")
      .setString("cc3cfa2a.ce0d.42e6.8060.31c147300ab4", "b1ae7176.2dc0.4dd5.9caf.31f84f8b5126")
      .setString("cc3cfa2a.ce0d.42e6.8264.6186e403c64e", "1cf84da1.bb53.488d.a929.e9a05d2d2352")
      .setBoolean("cc3cfa2a.7fa9.4f2d.abd0.54884e4cc1e2", true)
      .setBoolean("cc3cfa2a.7fa9.4ff6.aa54.e1a2d14f3933", false)
      .get();

  static Config getConfigFixture() {

    return CONFIG_FIXTURE;
  }
}
