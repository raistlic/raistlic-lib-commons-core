package org.raistlic.common.config.io;

import org.raistlic.common.config.core.Config;
import org.raistlic.common.config.core.ConfigBuilder;
import org.raistlic.common.config.core.ConfigFactory;
import org.raistlic.common.config.exception.ConfigIOException;
import org.raistlic.common.config.source.ConfigSource;
import org.raistlic.common.precondition.InvalidParameterException;
import org.raistlic.common.precondition.Precondition;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Lei Chen (2015-10-12)
 */
enum XmlConfigIO implements ConfigIO {

  INSTANCE;

  @Override
  public void writeConfig(ConfigSource config, OutputStream outputStream)
      throws InvalidParameterException, ConfigIOException {

    Precondition.param(config, "config").notNull();
    Precondition.param(outputStream, "outputStream").notNull();

    Configuration configuration = new Configuration();
    for (String key : config.getKeys()) {
      String value = config.getString(key);
      Entry entry = new Entry();
      entry.setKey(key);
      entry.setValue(value);
      configuration.getEntries().add(entry);
    }
    try {
      JAXBContext jaxbContext = JAXBContext.newInstance(Configuration.class);
      Marshaller marshaller = jaxbContext.createMarshaller();
      marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
      marshaller.marshal(configuration, outputStream);
    }
    catch (Exception ex) {
      throw new ConfigIOException(ex);
    }
  }

  @Override
  public Config readConfig(InputStream inputStream) throws ConfigIOException {

    Precondition.param(inputStream, "inputStream").notNull();

    ConfigBuilder configBuilder = ConfigFactory.newMutableConfig();
    readConfig(configBuilder, inputStream);
    return configBuilder.build();
  }

  @Override
  public void readConfig(ConfigBuilder configBuilder, InputStream inputStream) throws ConfigIOException {

    Precondition.param(configBuilder, "configBuilder").notNull();
    Precondition.param(inputStream, "inputStream").notNull();

    try {
      JAXBContext jaxbContext = JAXBContext.newInstance(Configuration.class);
      Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();
      @SuppressWarnings("unchecked")
      Configuration configuration = (Configuration) unmarshaller.unmarshal(inputStream);
      for (Entry entry : configuration.getEntries()) {
        configBuilder.setString(entry.getKey(), entry.getValue());
      }
    }
    catch (Exception ex) {
      throw new ConfigIOException(ex);
    }
  }

  @XmlRootElement(name = "configuration")
  @XmlAccessorType(XmlAccessType.FIELD)
  public static class Configuration {

    @XmlElement(name = "entry")
    @XmlElementWrapper(name = "entries")
    private List<Entry> entries = new ArrayList<Entry>();

    public List<Entry> getEntries() {

      return entries;
    }

    public void setEntries(List<Entry> entries) {

      this.entries = entries;
    }
  }

  @XmlAccessorType(XmlAccessType.FIELD)
  public static class Entry {

    @XmlElement
    private String key;

    @XmlElement
    private String value;

    public String getKey() {

      return key;
    }

    public void setKey(String key) {

      this.key = key;
    }

    public String getValue() {

      return value;
    }

    public void setValue(String value) {

      this.value = value;
    }
  }
}
