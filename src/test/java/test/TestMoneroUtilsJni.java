package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import monero.utils.MoneroUtilsJni;

/**
 * Test bridged C++ utilities.
 */
public class TestMoneroUtilsJni {

  // Can serialize heights with small numbers
  @Test
  public void testSerializeHeightsSmall() {
    Map<String, Object> map = new HashMap<String, Object>();
    map.put("heights", Arrays.asList(111, 222, 333));
    byte[] binary = MoneroUtilsJni.mapToBinary(map);
    assertTrue(binary.length > 0);
    //for (int i = 0; i < binary.length; i++) System.out.println(binary[i]);
    Map<String, Object> map2 = MoneroUtilsJni.binaryToMap(binary);
    assertEquals(map, map2);
  };
  
  // Can serialize heights with big numbers
  @Test
  public void testSerializeHeightsBig() {
    Map<String, Object> map = new HashMap<String, Object>();
    map.put("heights", Arrays.asList(123456, 1234567, 870987));
    byte[] binary = MoneroUtilsJni.mapToBinary(map);
    assertTrue(binary.length > 0);
    Map<String, Object> map2 = MoneroUtilsJni.binaryToMap(binary);
    assertEquals(map, map2);
  }
  
  // Can serialize map with text
  @Test
  public void testSerializeTextShort() {
    Map<String, Object> map = new HashMap<String, Object>();
    map.put("msg", "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
    byte[] binary = MoneroUtilsJni.mapToBinary(map);
    assertTrue(binary.length > 0);
    Map<String, Object> map2 = MoneroUtilsJni.binaryToMap(binary);
    assertEquals(map, map2);
  }
  
  // Can serialize json with long text
  @Test
  public void testSerializeTextLong() {
    Map<String, Object> map = new HashMap<String, Object>();
    map.put("msg", "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" +
            "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" + 
            "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" + 
            "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" + 
            "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" + 
            "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" + 
            "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" + 
            "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" + 
            "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" + 
            "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" + 
            "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" + 
            "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" + 
            "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" + 
            "Hello there my good man lets make a nice long text to test with lots of exclamation marks!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n");
    byte[] binary = MoneroUtilsJni.mapToBinary(map);
    assertTrue(binary.length > 0);
    Map<String, Object> map2 = MoneroUtilsJni.binaryToMap(binary);
    assertEquals(map, map2);
  }
}
