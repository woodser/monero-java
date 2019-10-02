package common.utils;

import java.util.ArrayList;
import java.util.List;

/**
 * Collection of general purpose utilities.
 */
public class GenUtils {
  
  public static final int[] EMPTY_INT_ARRAY = new int[0];
  
  /**
   * Asserts that the given boolean is true.
   * 
   * @param val is the boolean to assert as true
   */
  public static void assertTrue(boolean val) {
    if (!val) throw new AssertionError("value asserted as true but was false");
  }
  
  /**
   * Asserts that the given boolean is true.
   * 
   * @param failMsg is the failure message if the booelean is not true
   * @param val is the boolean to assert as true
   */
  public static void assertTrue(String failMsg, boolean val) {
    if (!val) throw new AssertionError(failMsg);
  }
  
  /**
   * Asserts that the given boolean is false.
   * 
   * @param val is the boolean to assert as false
   */
  public static void assertFalse(boolean val) {
    if (val) throw new AssertionError("value asserted as false but was true");
  }
  
  /**
   * Asserts that the given boolean is false.
   * 
   * @param failMsg is the failure message if the booelean is not false
   * @param val is the boolean to assert as false
   */
  public static void assertFalse(String failMsg, boolean val) {
    if (val) throw new AssertionError(failMsg);
  }
  
  /**
   * Asserts that the given value is null.
   * 
   * @param val is the value to assert as null
   */
  public static void assertNull(Object val) {
    if (val != null) throw new AssertionError("value asserted as null but was not null");
  }
  
  /**
   * Asserts that the given value is null.
   * 
   * @param failMsg is the failure message if the value is not null
   * @param val is the value to assert as null
   */
  public static void assertNull(String failMsg, Object val) {
    if (val != null) throw new AssertionError(failMsg);
  }
  
  /**
   * Asserts that the given value is not null.
   * 
   * @param val is the value to assert as not null
   */
  public static void assertNotNull(Object val) {
    if (val == null) throw new AssertionError("value asserted as not null but was null");
  }
  
  /**
   * Asserts that the given value is not null.
   * 
   * @param failMsg is the failure message if the value is null
   * @param val is the value to assert as not null
   */
  public static void assertNotNull(String failMsg, Object val) {
    if (val == null) throw new AssertionError(failMsg);
  }
  
  /**
   * Asserts that the given values are equal.  Throws an exception if not equal.
   * 
   * @param val1 is a value to assert as equal
   * @param val2 is a value to assert as equal
   */
  public static void assertEquals(Object val1, Object val2) {
    if (val1 == null && val2 == null) return;
    assertFalse(val1 + " != " + val2, val1 == null || val2 == null);
    assertTrue(val1 + " != " + val2, val1.equals(val2));
  }
  
  /**
   * Asserts that the given values are equal.  Throws an exception if not equal.
   * 
   * @param failMsg is the failure message if the values are not equal
   * @param val1 is a value to assert as equal
   * @param val2 is a value to assert as equal
   */
  public static void assertEquals(String failMsg, Object val1, Object val2) {
    try {
      assertEquals(val1, val2);
    } catch (Exception e) {
      throw new AssertionError(failMsg);
    }
  }
  
  /**
   * Asserts that the given values are not equal.  Throws an exception if equal.
   * 
   * @param val1 is a value to assert as not equal
   * @param val2 is a value to assert as not equal
   */
  public static void assertNotEquals(Object val1, Object val2) {
    assertFalse(val1 == null && val2 == null);
    assertTrue(val1 + " equals " + val2, val1 == null || val2 == null || !val1.equals(val2));
  }
  
  /**
   * Asserts that the given values are not equal.  Throws an exception if equal.
   * 
   * @param failMsg is the failure message if the values are equal
   * @param val1 is a value to assert as not equal
   * @param val2 is a value to assert as not equal
   */
  public static void assertNotEquals(String failMsg, Object val1, Object val2) {
    try {
      assertNotEquals(val1, val2);
    } catch (Exception e) {
      throw new AssertionError(failMsg);
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
  
  /**
   * Produces a new array containing the elements between
   * the start and end indices.
   * <p>
   * Code from <a href="https://commons.apache.org/proper/commons-lang/">Apache Commons Lang</a>.
   */
  public static int[] subarray(final int[] array, int startIndexInclusive, int endIndexExclusive) {
    if (array == null) {
        return null;
    }
    if (startIndexInclusive < 0) {
        startIndexInclusive = 0;
    }
    if (endIndexExclusive > array.length) {
        endIndexExclusive = array.length;
    }
    final int newSize = endIndexExclusive - startIndexInclusive;
    if (newSize <= 0) {
        return EMPTY_INT_ARRAY;
    }

    final int[] subarray = new int[newSize];
    System.arraycopy(array, startIndexInclusive, subarray, 0, newSize);
    return subarray;
  }
}
