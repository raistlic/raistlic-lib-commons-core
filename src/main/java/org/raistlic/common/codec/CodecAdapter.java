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

package org.raistlic.common.codec;

import org.raistlic.common.precondition.Precondition;

/**
 * This class simply combines a proper encoder and a proper decoder together,
 * to adapt the {@link org.raistlic.common.codec.Codec} interface.
 */
public class CodecAdapter<S, D> implements Codec<S, D> {

  private final Encoder<? super S, D> encoder;

  private final Decoder<S, ? super D> decoder;

  /**
   * Defines a CodecAdapter with the specified encoder and decoder.
   *
   * @param encoder the encoder as a component of the adapter to define.
   * @param decoder the decoder as a component of the adapter to define.
   * @throws org.raistlic.common.precondition.InvalidParameterException if the specified
   *                                                                    {@code encoder} or {@code decoder} is {@code null}.
   */
  public CodecAdapter(Encoder<? super S, D> encoder,
                      Decoder<S, ? super D> decoder) {

    Precondition.param(encoder).isNotNull();
    Precondition.param(decoder).isNotNull();

    this.encoder = encoder;
    this.decoder = decoder;
  }

  @Override
  public D encode(S src) {

    return encoder.encode(src);
  }

  @Override
  public S decode(D target) {

    return decoder.decode(target);
  }
}
