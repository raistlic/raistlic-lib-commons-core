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
 *
 * @author Lei.C
 */
public abstract class DecoderCodecAdapter<S, D> implements Codec<S, D> {
  
  private Decoder<S, ? super D> decoder;
  
  public DecoderCodecAdapter(Decoder<S, ? super D> decoder) {
    
    if( decoder == null )
      throw new NullPointerException("decoder is null.");
    
    this.decoder = decoder;
  }
  
  @Override
  public final boolean isValidDest(D dest) {
    
    return decoder.isValidDest(dest);
  }

  @Override
  public final S decode(D dest) {
    
    return decoder.decode(dest);
  }
}
