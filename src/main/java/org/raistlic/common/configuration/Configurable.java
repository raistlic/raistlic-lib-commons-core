package org.raistlic.common.configuration;

/**
 * @author lei.c
 * @since 2014-12-28
 */
public interface Configurable {

  void applyConfig(Configuration configuration);

  void extractConfig(Configuration.Builder builder);
}
