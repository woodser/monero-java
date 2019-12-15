package common.utils;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Collection of general purpose utilities.
 */
public class GenUtils {
  
  private static final int[] EMPTY_INT_ARRAY = new int[0];
  
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
   * @param expected is the expected value to assert as equal
   * @param actual is the actual value to assert as equal
   */
  public static void assertEquals(Object expected, Object actual) {
    if (expected == null && actual == null) return;
    assertFalse(expected + " != " + actual, expected == null || actual == null);
    assertTrue(expected + " != " + actual, expected.equals(actual));
  }
  
  /**
   * Asserts that the given values are equal.  Throws an exception if not equal.
   * 
   * @param failMsg is the failure message if the values are not equal
   * @param expected is the expected value to assert as equal
   * @param actual is the actual value to assert as equal
   */
  public static void assertEquals(String failMsg, Object expected, Object actual) {
    try {
      assertEquals(expected, actual);
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
   * @return an indentation string of the given length
   */
  public static String getIndent(int length) {
    String str = "";
    for (int i = 0; i < length; i++) str += "  "; // two spaces
    return str;
  }
  
  /**
   * Produces a new array containing the elements between the start and end indices.
   * <p>
   * Code from <a href="https://commons.apache.org/proper/commons-lang/">Apache Commons Lang</a>.
   * 
   * @param array is the array to derive a subarray from
   * @param startIndexInclusive is the start index of the subarray, inclusive
   * @param endIndexExclusive is the end index of the subarray, exclusive
   * @return the subarray
   */
  public static int[] subarray(final int[] array, int startIndexInclusive, int endIndexExclusive) {
    if (array == null) return null;
    if (startIndexInclusive < 0) startIndexInclusive = 0;
    if (endIndexExclusive > array.length) endIndexExclusive = array.length;
    
    final int newSize = endIndexExclusive - startIndexInclusive;
    if (newSize <= 0) return EMPTY_INT_ARRAY;

    final int[] subarray = new int[newSize];
    System.arraycopy(array, startIndexInclusive, subarray, 0, newSize);
    return subarray;
  }
  
  /**
   * Convenience method to reconcile two values with default configuration by
   * calling reconcile(val1, val2, null, null, null).
   * 
   * @param val1 is a value to reconcile
   * @param val2 is a value to reconcile
   * @return the reconciled value if reconcilable, throws exception otherwise
   */
  public static <T> T reconcile(T val1, T val2) {
    return reconcile(val1, val2, null, null, null);
  }
  
  /**
   * Reconciles two values.
   * 
   * @param val1 is a value to reconcile
   * @param val2 is a value to reconcile
   * @param resolveDefined uses defined value if true or null, null if false
   * @param resolveTrue uses true over false if true, false over true if false, must be equal if null
   * @param resolveMax uses max over min if true, min over max if false, must be equal if null
   * @return the reconciled value if reconcilable, throws exception otherwise
   */
  @SuppressWarnings("unchecked")
  public static <T> T reconcile(T val1, T val2, Boolean resolveDefined, Boolean resolveTrue, Boolean resolveMax) {
    
    // check for same reference
    if (val1 == val2) return val1;
    
    // check for BigInteger equality
    Integer comparison = null; // save comparison for later if applicable
    if (val1 instanceof BigInteger && val2 instanceof BigInteger) {
      comparison = ((BigInteger) val1).compareTo((BigInteger) val2);  
      if (comparison == 0) return val1;
    }
    
    // resolve one value null
    if (val1 == null || val2 == null) {
      if (Boolean.FALSE.equals(resolveDefined)) return null;  // use null
      else return val1 == null ? val2 : val1;  // use defined value
    }
    
    // resolve different booleans
    if (resolveTrue != null && Boolean.class.isInstance(val1) && Boolean.class.isInstance(val2)) {
      return (T) resolveTrue;
    }
    
    // resolve different numbers
    if (resolveMax != null) {
      
      // resolve BigIntegers
      if (val1 instanceof BigInteger && val2 instanceof BigInteger) {
        return resolveMax ? (comparison < 0 ? val2 : val1) : (comparison < 0 ? val1 : val2);
      }
      
      // resolve integers
      if (val1 instanceof Integer && val2 instanceof Integer) {
        return (T) (Integer) (resolveMax ? Math.max((Integer) val1, (Integer) val2) : Math.min((Integer) val1, (Integer) val2));
      }
      
      // resolve longs
      if (val1 instanceof Long && val2 instanceof Long) {
        return (T) (Long) (resolveMax ? Math.max((Long) val1, (Long) val2) : Math.min((Long) val1, (Long) val2));
      }

      throw new RuntimeException("Need to resolve primitives and object versions");
//      // resolve js numbers
//      if (typeof val1 === "number" && typeof val2 === "number") {
//        return config.resolveMax ? Math.max(val1, val2) : Math.min(val1, val2);
//      }
    }
    
    // assert deep equality
    GenUtils.assertEquals("Cannot reconcile values " + val1 + " and " + val2 + " with config: [" + resolveDefined + ", " + resolveTrue + ", " + resolveMax + "]", val1, val2);
    return val1;
  }
  
  /**
   * Reconciles two int arrays.  The arrays must be identical or an exception is thrown.
   * 
   * @param arr1 is an array to reconcile
   * @param arr2 is an array to reconcile
   * @return T[] is the reconciled array
   */
  public static int[] reconcileIntArrays(int[] arr1, int[] arr2) {
    
    // check for same reference or null
    if (arr1 == arr2) return arr1;
    
    // resolve one value defined
    if (arr1 == null || arr2 == null) {
      return arr1 == null ? arr2 : arr1;
    }
    
    // assert deep equality
    GenUtils.assertTrue("Cannot reconcile arrays", Arrays.equals(arr1, arr2));
    return arr1;
  }
  
  /**
   * Returns a human-friendly key value line.
   * 
   * @param key is the key
   * @param value is the value
   * @param indent indents the line
   * @return the human-friendly key value line
   */
  public static String kvLine(Object key, Object value, int indent) {
    return kvLine(key, value, indent, true, true);
  }
  
  /**
   * Returns a human-friendly key value line.
   * 
   * @param key is the key
   * @param value is the value
   * @param indent indents the line
   * @param newline specifies if the string should be terminated with a newline or not
   * @param ignoreUndefined specifies if undefined values should return an empty string
   * @return the human-friendly key value line
   */
  public static String kvLine(Object key, Object value, int indent, boolean newline, boolean ignoreUndefined) {
    if (value == null && ignoreUndefined) return "";
    return GenUtils.getIndent(indent) + key + ": " + value + (newline ? '\n' : "");
  }
}
