package utils;

import java.util.Map;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

import types.JsonException;

/**
 * Collection of utilities for working with JSON.
 * 
 * @author woodser
 */
public class JsonUtils {
  
  // set up jackson object mapper
  private static final ObjectMapper MAPPER;
  static {
    MAPPER = new ObjectMapper();
    MAPPER.setSerializationInclusion(Include.NON_NULL);
  }
  
  /**
   * Serializes an object to a JSON string.
   * 
   * @param obj is the object to serialize
   * @return String is the object serialized to a JSON string
   */
  public static String serialize(Object obj) {
    try {
      return MAPPER.writeValueAsString(obj);
    } catch (Exception e) {
      throw new JsonException("Error serializing object", e);
    }
  }
  
  /**
   * Deserializes JSON to a specific class.
   * 
   * @param json is the JSON to deserialize
   * @param clazz specifies the class to deserialize to
   * @return T is the object deserialized from JSON to the given class
   */
  public static <T> T deserialize(String json, Class<T> clazz) {
    try {
      return MAPPER.readValue(json, clazz);
    } catch (Exception e) {
      throw new JsonException("Error deserializing json to class", e);
    }
  }
  
  /**
   * Deserializes JSON to a parameterized type.
   * 
   * @param json is the JSON to deserialize
   * @param type is the parameterized type to deserialize to (e.g. new TypeReference<Map<String, Object>>(){})
   * @return T is the object deserialized from JSON to the given parameterized type
   */
  @SuppressWarnings("unchecked")
  public static <T> T deserialize(String json, TypeReference<T> type) {
    try {
      return (T) MAPPER.readValue(json, type);
    } catch (Exception e) {
      throw new JsonException("Error deserializing json to parameterized type", e);
    }
  }

  /**
   * Converts a JSON string to a map.
   * 
   * @param json is the string to convert to a map
   * @return Map<String, Object> is the json string converted to a map
   */
  public static Map<String, Object> toMap(String json) {
    return deserialize(json, new TypeReference<Map<String, Object>>(){});
  }
  
  /**
   * Converts an object to a map.
   * 
   * @param obj is the object to a convert to a map
   * @return Map<String, Object> is the object converted to a map
   */
  public static Map<String, Object> toMap(Object obj) {
    return toMap(serialize(obj));
  }
}
