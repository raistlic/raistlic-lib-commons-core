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

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * The annotation is to mark a static factory method or constructor that is preferred to be used
 * as the method to create a config entity instance.
 * <p>
 * The annotated static factory method or constructor must have all its parameters marked with
 * {@link ConfigProperty}, unless no parameters needed.
 * <p>
 * Although not restricted, it is preferred that only one of the static factory methods and constructors
 * being marked as {@link ConfigConstructor}, or otherwise which one to be used is undefined.
 *
 * @author Lei CHEN (2015-12-22)
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.CONSTRUCTOR, ElementType.METHOD})
public @interface ConfigConstructor {

}
