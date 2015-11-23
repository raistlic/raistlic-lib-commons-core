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

package org.raistlic.common.precondition;

import java.util.function.Function;

/**
 * @author Lei Chen (2015-10-14)
 */
abstract class ExceptionProviders {

  static Function<String, InvalidParameterException> invalidParameterExceptionProvider() {

    return InvalidParameterExceptionProvider.INSTANCE;
  }

  static Function<String, InvalidStateException> invalidStateExceptionProvider() {

    return InvalidStateExceptionProvider.INSTANCE;
  }

  static Function<String, InvalidContextException> invalidContextExceptionProvider() {

    return InvalidContextExceptionProvider.INSTANCE;
  }

  private enum InvalidParameterExceptionProvider implements Function<String, InvalidParameterException> {

    INSTANCE;

    @Override
    public InvalidParameterException apply(String message) {

      return new InvalidParameterException(message);
    }
  }

  private enum InvalidStateExceptionProvider implements Function<String, InvalidStateException> {

    INSTANCE;

    @Override
    public InvalidStateException apply(String message) {

      return new InvalidStateException(message);
    }
  }

  private enum InvalidContextExceptionProvider implements Function<String, InvalidContextException> {

    INSTANCE;

    @Override
    public InvalidContextException apply(String message) {

      throw new InvalidContextException(message);
    }
  }

  private ExceptionProviders() { }
}
