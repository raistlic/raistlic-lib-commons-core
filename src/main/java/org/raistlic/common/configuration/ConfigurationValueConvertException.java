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

package org.raistlic.common.configuration;

/**
 * @author lei.c
 * @since 2014-12-28
 */
public class ConfigurationValueConvertException extends ConfigurationException {

  protected ConfigurationValueConvertException(String message) {

    super(message);
  }

  protected ConfigurationValueConvertException(Throwable cause) {

    super(cause);
  }

  protected ConfigurationValueConvertException(String message, Throwable cause) {

    super(message, cause);
  }
}
