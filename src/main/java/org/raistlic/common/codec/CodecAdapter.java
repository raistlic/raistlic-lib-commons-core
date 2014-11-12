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
 * This class simply combines a proper encoder and a proper decoder together,
 * to adapt the {@link org.raistlic.common.codec.Codec} interface.
 *
 * @author Lei.C
 */
public class CodecAdapter<S, D> implements Codec<S, D> {
  
  private Encoder<? super S, D> encoder;
  private Decoder<S, ? super D> decoder;
  
  /**
   * Defines a CodecAdapter with the specified encoder and decoder.
   * 
   * @param encoder the encoder as a component of the adapter to define.
   * 
   * @param decoder the decoder as a component of the adapter to define.
   * 
   * @throws NullPointerException if the specified {@code encoder} or {@code 
   *         decoder} is {@code null}.
   */
  public CodecAdapter(Encoder<? super S, D> encoder,
                      Decoder<S, ? super D> decoder) {
    
    if( encoder == null )
      throw new NullPointerException("encoder is null.");
    
    if( decoder == null )
      throw new NullPointerException("decoder is null.");
    
    this.encoder = encoder;
    this.decoder = decoder;
  }
  
  @Override
  public boolean isValidSrc(S src) {
    
    return encoder.isValidSrc(src);
  }

  @Override
  public D encode(S src) {
    
    return encoder.encode(src);
  }

  @Override
  public boolean isValidDest(D dest) {
    
    return decoder.isValidDest(dest);
  }

  @Override
  public S decode(D dest) {
    
    return decoder.decode(dest);
  }
}
