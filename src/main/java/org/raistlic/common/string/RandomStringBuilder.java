/*
 * Copyright 2016 Lei Chen (raistlic@gmail.com)
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.raistlic.common.string;

import org.raistlic.common.util.ObjectBuilder;

public interface RandomStringBuilder extends ObjectBuilder<String> {

  int getMinimumLength();

  int getMaximumLength();

  RandomStringBuilder withMinimumLength(int minimumLength);

  RandomStringBuilder withMaximumLength(int maximumLength);

  RandomStringBuilder withLength(int length);

  RandomStringBuilder withCharSet(String charset);
}
