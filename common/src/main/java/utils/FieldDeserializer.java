package utils;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;

/**
 * Deserializes specific fields to specific classes.
 * 
 * @author woodser
 */
public class FieldDeserializer extends JsonDeserializer<Map<String, Object>> {
  
  private Map<String, Class<?>> fieldClasses;
  
  /**
   * Constructs the deserializer with the given field names.
   * 
   * @param fieldNames specifies the names of fields to deserialize to specific classes
   */
  public FieldDeserializer(Map<String, Class<?>> fieldClasses) {
    super();
    this.fieldClasses = fieldClasses;
  }

  @Override
  public Map<String, Object> deserialize(JsonParser jp, DeserializationContext ctxt) throws IOException, JsonProcessingException {
    Map<String, Object> result = new HashMap<String, Object>();
    jp.nextToken();
    while (!JsonToken.END_OBJECT.equals(jp.getCurrentToken())) {
      String key = jp.getText();
      jp.nextToken();
      Class<?> clazz = fieldClasses.get(key);
      if (clazz != null) {
        result.put(key, jp.readValueAs(clazz));
      } else {
        if (JsonToken.START_OBJECT.equals(jp.getCurrentToken())) {
          result.put(key, deserialize(jp, ctxt));
        } else if (JsonToken.START_ARRAY.equals(jp.getCurrentToken())) {
          jp.nextToken();
          List<Object> list = new ArrayList<Object>();
          while (!JsonToken.END_ARRAY.equals(jp.getCurrentToken())) {
            list.add(deserialize(jp, ctxt));
            jp.nextToken();
          }
          result.put(key, list);
        } else {
          result.put(key, jp.readValueAs(Object.class));
        }
      }
      jp.nextToken();
    }
    return result;
  }
}
