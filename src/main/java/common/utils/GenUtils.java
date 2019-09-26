package common.utils;

import java.util.ArrayList;
import java.util.List;

/**
 * Collection of general purpose utilities.
 */
public class GenUtils {
  
  /**
   * Asserts that the given boolean is true.
   * 
   * @param val is the boolean to assert as true
   */
  public static void assertTrue(boolean val) {
    assert(val);
  }
  
  /**
   * Asserts that the given boolean is true.
   * 
   * @param failMsg is the failure message if the booelean is not true
   * @param val is the boolean to assert as true
   */
  public static void assertTrue(String failMsg, boolean val) {
    if (!val) throw new RuntimeException(failMsg);
  }
  
  public static void assertFalse(boolean val) {
    assert(!val);
  }
  
  public static void assertFalse(String failMsg, boolean val) {
    if (val) throw new RuntimeException(failMsg);
  }
  
  public static void assertNull(Object val) {
    assert(val == null);
  }
  
  public static void assertNull(String failMsg, Object val) {
    if (val != null) throw new RuntimeException(failMsg);
  }
  
  public static void assertNotNull(Object val) {
    assert(val != null);
  }
  
  public static void assertNotNull(String failMsg, Object val) {
    if (val == null) throw new RuntimeException(failMsg);
  }
  
  public static void assertEquals(Object val1, Object val2) {
    if (val1 == null && val2 == null) return;
    assertFalse(val1 + " != " + val2, val1 == null || val2 == null);
    assertTrue(val1 + " != " + val2, val1.equals(val2));
  }
  
  public static void assertEquals(String failMsg, Object val1, Object val2) {
    try {
      GenUtils.assertEquals(val1, val2);
    } catch (Exception e) {
      throw new RuntimeException(failMsg);
    }
  }
  
  public static void assertNotEquals(Object val1, Object val2) {
    assertFalse(val1 == null && val2 == null);
    assertTrue(val1 + " equals " + val2, val1 == null || val2 == null || !val1.equals(val2));
  }
  
  public static void assertNotEquals(String failMsg, Object val1, Object val2) {
    try {
      assertNotEquals(val1, val2);
    } catch (Exception e) {
      throw new RuntimeException(failMsg);
    }
  }
  
  /**
   * Converts a templated array to a list.
   * 
   * @param arr is an array of type T to convert to a list
   * @return List<T> is the array converted to a list
   */
  public static <T> List<T> arrayToList(T[] arr) {
    List<T> list = new ArrayList<T>(arr.length);
    for (T elem : arr) list.add(elem);
    return list;
  }
  
  /**
   * Converts a list of integers to an int array.
   * 
   * @param list is the list ot convert
   * @return the int array
   */
  public static int[] listToIntArray(List<Integer> list) {
    if (list == null) return null;
    int[] ints = new int[list.size()];
    for (int i = 0; i < list.size(); i++) ints[i] = list.get(i);
    return ints;
  }
  
  /**
   * Returns a string indentation of the given length;
   * 
   * @param length is the length of the indentation
   * @returns {string} is an indentation string of the given length
   */
  public static String getIndent(int length) {
    String str = "";
    for (int i = 0; i < length; i++) str += "  "; // two spaces
    return str;
  }
}
