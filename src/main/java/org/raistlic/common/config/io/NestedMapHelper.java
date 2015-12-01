package org.raistlic.common.config.io;

import org.raistlic.common.config.core.ConfigBuilder;
import org.raistlic.common.config.source.ConfigSource;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Lei Chen (2015-10-12)
 */
final class NestedMapHelper {

  @SuppressWarnings("unchecked")
  static Map<String, Object> configToMap(ConfigSource config) {

    Map<String, Object> result = new HashMap<String, Object>();
    for (String key : config.getKeys()) {

      String[] keyTokens = key.split("\\.");
      Map<String, Object> currMap = result;
      for (int i = 0; i < keyTokens.length - 1; i++) {
        String token = keyTokens[i];
        Object nextLevel = currMap.get(token);
        if (! (nextLevel instanceof Map)) {
          nextLevel = new HashMap<String, Object>();
          currMap.put(token, nextLevel);
        }
        currMap = (Map<String, Object>) nextLevel;
      }
      currMap.put(keyTokens[keyTokens.length - 1], config.getString(key));
    }
    return result;
  }

  static void mapToConfig(Map<String, Object> jsonMap, String prefix, ConfigBuilder configBuilder) {

    for (Map.Entry<String, Object> entry : jsonMap.entrySet()) {

      String key = entry.getKey();
      key = prefix.isEmpty() ? key : prefix + "." + key;
      Object val = entry.getValue();
      if (val instanceof Map) {
        @SuppressWarnings("unchecked")
        Map<String, Object> converted = (Map<String, Object>) val;
        mapToConfig(converted, key, configBuilder);
      }
      else {
        configBuilder.setString(key, val == null ? null : val.toString());
      }
    }
  }
}
