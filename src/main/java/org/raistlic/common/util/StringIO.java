package org.raistlic.common.util;

import org.raistlic.common.precondition.Precondition;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * The input/output helper methods for {@link String} .
 *
 * @author Lei Chen (2015-10-12)
 */
public final class StringIO {

  /**
   * The method reads all input from the input stream, as a single {@link String} .
   *
   * @param inputStream the input stream to read from, cannot be {@code null} and must be open for
   *                    read.
   * @return the {@link String} content read, or empty string in case nothing from the input stream,
   *         the method never returns {@code null}.
   *
   * @throws org.raistlic.common.precondition.InvalidParameterException when {@code inputStream} is
   *         {@code null}.
   * @throws IOException when anything goes wrong in the process of reading the string.
   */
  public static String readAll(InputStream inputStream) throws IOException {

    Precondition.param(inputStream, "inputStream").isNotNull();

    ByteArrayOutputStream bytes = new ByteArrayOutputStream();
    int read;
    do {
      read = inputStream.read(BUFFER);
      if (read > 0) {
        bytes.write(BUFFER, 0, read);
      }
      read = inputStream.read(BUFFER);
    } while (read == BUFFER_SIZE);
    return new String(bytes.toByteArray());
  }

  private static final int BUFFER_SIZE = 4096;

  private static final byte[] BUFFER = new byte[BUFFER_SIZE];
}
