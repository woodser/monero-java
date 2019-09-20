package common.utils;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

/**
 * Collection of utilities for working with streams.
 * 
 * @author woodser
 */
public class StreamUtils {
  
  /**
   * Converts a byte array to an input stream.
   * 
   * @param bytes is the byte[] to convert to an input stream
   * @return InputStream is the input stream initialized from the byte array
   */
  public static InputStream bytesToStream(byte[] bytes) {
    return new ByteArrayInputStream(bytes);
  }
}
