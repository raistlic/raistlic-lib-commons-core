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
 * This class defines an unchecked exception, to be thrown when unexpected error occurs during
 * the process of encoding or decoding.
 *
 * @author Lei.C
 */
public class ValueConversionException extends RuntimeException {
  
  /**
   * Constructs a new {@code CodingException} with null as its detail message.
   */
  public ValueConversionException() {
    
    super();
  }
  
  /**
   * Constructs a new {@code CodingException} with the specified detail message.
   * 
   * @param message the detail message (which is saved for later retrieval by the 
   *        {@link Throwable#getMessage() method).
   */
  public ValueConversionException(String message) {
    
    super(message);
  }
  
  /**
   * Constructs a new {@code CodingException} with the specified cause.
   * 
   * @param cause the cause (which is saved for later retrieval by the 
   *        {@link Throwable#getCause() method). (A null value is
   *        permitted, and indicates that the cause is nonexistent or unknown.)
   */
  public ValueConversionException(Throwable cause) {
    
    super(cause);
  }
  
  /**
   * Constructs a new {@code CodingException} with null as its detail message 
   * and cause.
   * 
   * @param message the detail message (which is saved for later retrieval by the 
   *        {@link Throwable#getMessage() method).
   * 
   * @param cause the cause (which is saved for later retrieval by the 
   *        {@link Throwable#getCause() method). (A null value is
   *        permitted, and indicates that the cause is nonexistent or unknown.)
   */
  public ValueConversionException(String message, Throwable cause) {
    
    super(message, cause);
  }
}
