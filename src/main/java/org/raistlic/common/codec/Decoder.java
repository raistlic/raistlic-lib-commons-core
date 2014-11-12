/*
 * Copyright 2013 Lei CHEN (raistlic@gmail.com)
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

package org.raistlic.common.codec;

/**
 * This instance defines the parser that parses the referenced type {@code D} to
 * the referenced type {@code S}.
 *
 * @author Lei.C
 */
public interface Decoder<S, D> {
  
  public boolean isValidDest(D dest);
  
  /**
   * 
   * 
   * @param dest
   * @return
   */
  public S decode(D dest);
}
