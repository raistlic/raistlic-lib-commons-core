package org.raistlic.common.config.io;

/**
 * @author Lei Chen (2015-09-14)
 */
public final class ConfigIOFactory {

  public static ConfigIO getPropertiesConfigIO() {

    return PropertiesConfigIO.INSTANCE;
  }

  public static ConfigIO getXmlConfigIO() {

    return XmlConfigIO.INSTANCE;
  }

  public static ConfigIO getJsonConfigIO() {

    return JsonConfigIO.INSTANCE;
  }

  public static ConfigIO getYamlConfigIO() {

    return YamlConfigIO.INSTANCE;
  }
}
