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

import java.lang.annotation.*;

/**
 * The annotation is used to mark the members or parameters that are values to be fetched or
 * entities to be created from the configuration context.
 *
 * @author Lei CHEN (2015-12-21)
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD, ElementType.PARAMETER, ElementType.METHOD})
public @interface ConfigProperty {

  /**
   * When {@code ""} , use the annotated field or method name, cannot be {@code ""} when annotated
   * on parameters.
   *
   * @return configuration property name or part.
   */
  String value() default "";
}
