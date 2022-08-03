package com.mingshi.skyflying.utils;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.codehaus.jackson.map.DeserializationConfig;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.map.SerializationConfig;
import org.codehaus.jackson.map.annotate.JsonSerialize.Inclusion;
import org.codehaus.jackson.type.JavaType;
import org.codehaus.jackson.type.TypeReference;

import java.text.SimpleDateFormat;

/**
 * Created by geely
 */
@Slf4j
public class JsonUtil {

  private static org.codehaus.jackson.map.ObjectMapper objectMapper = new ObjectMapper();
  private static com.fasterxml.jackson.databind.ObjectMapper mapper = new com.fasterxml.jackson.databind.ObjectMapper();
  private static com.fasterxml.jackson.databind.ObjectMapper mapperWithYMDDate = new com.fasterxml.jackson.databind.ObjectMapper();

  static {
    //对象的所有字段全部列入
    objectMapper.setSerializationInclusion(Inclusion.ALWAYS);

    //取消默认转换timestamps形式
    objectMapper.configure(SerializationConfig.Feature.WRITE_DATES_AS_TIMESTAMPS, false);

    //忽略空Bean转json的错误
    objectMapper.configure(SerializationConfig.Feature.FAIL_ON_EMPTY_BEANS, false);

    //所有的日期格式都统一为以下的样式，即yyyy-MM-dd HH:mm:ss
    objectMapper.setDateFormat(new SimpleDateFormat("yyyy-MM-dd HH:mm:ss"));

    //忽略 在json字符串中存在，但是在java对象中不存在对应属性的情况。防止错误
    objectMapper.configure(DeserializationConfig.Feature.FAIL_ON_UNKNOWN_PROPERTIES, false);
  }

  /**
   * <B>方法名称：createJSONObject</B>
   * <B>概要说明：创建JSONObject实例</B>
   *
   * @return ObjectNode
   * @Author zm
   * @Date 2022年06月16日 08:06:56
   * @Param []
   **/
  public static ObjectNode createJSONObject() {
    ObjectNode objectNode = mapper.createObjectNode();
    return objectNode;
  }

  /**
   * <B>方法名称：createJSONArray</B>
   * <B>概要说明：创建JSONArray实例</B>
   *
   * @return com.fasterxml.jackson.databind.node.ArrayNode
   * @Author zm
   * @Date 2022年06月16日 08:06:35
   * @Param []
   **/
  public static ArrayNode createJSONArray() {
    ArrayNode arrayNode = mapper.createArrayNode();
    return arrayNode;
  }

  public static <T> String object2String(T obj) {
    if (obj == null) {
      return null;
    }
    try {
      return obj instanceof String ? (String) obj : mapper.writeValueAsString(obj);
    } catch (JsonProcessingException e) {
      log.error(" # JsonUtil.createJSONArray() # 将实例【{}】转换成字符串时，出现了异常。 ", obj.toString());
      return null;
    }
  }

  public static <T> String obj2String(T obj) {
    if (obj == null) {
      return null;
    }
    try {
      return obj instanceof String ? (String) obj : objectMapper.writeValueAsString(obj);
    } catch (Exception e) {
      return object2String(obj);
    }
  }

  public static <T> String obj2StringPretty(T obj) {
    if (obj == null) {
      return null;
    }
    try {
      return obj instanceof String ? (String) obj : objectMapper.writerWithDefaultPrettyPrinter().writeValueAsString(obj);
    } catch (Exception e) {
      try {
        return mapper.writerWithDefaultPrettyPrinter().writeValueAsString(obj);
      } catch (JsonProcessingException ex) {
        log.error(" # JsonUtil.obj2StringPretty() # 将实例【{}】转换成字符串时，出现了异常。 ", obj.toString());
        return null;
      }
    }
  }


  public static <T> T string2Object(String str, Class<T> clz) {
    try {
      return mapper.readValue(str == null ? "{}" : str, clz);
    } catch (Exception e) {
      throw new RuntimeException("json parse to object [" + clz + "] error:" + str, e);
    }
  }

  public static <T> T string2Obj(String str, Class<T> clazz) {
    if (StringUtils.isEmpty(str) || clazz == null) {
      return null;
    }

    try {
      return clazz.equals(String.class) ? (T) str : objectMapper.readValue(str, clazz);
    } catch (Exception e) {
      return string2Object(str, clazz);
    }
  }


  public static <T> T string2Obj(String str, TypeReference<T> typeReference) {
    if (StringUtils.isEmpty(str) || typeReference == null) {
      return null;
    }
    try {
      return (T) (typeReference.getType().equals(String.class) ? str : objectMapper.readValue(str, typeReference));
    } catch (Exception e) {
      log.warn("Parse String to Object error", e);
      return null;
    }
  }

  public static <T> T string2Obj(String str, Class<?> collectionClass, Class<?>... elementClasses) {
    JavaType javaType = objectMapper.getTypeFactory().constructParametricType(collectionClass, elementClasses);
    try {
      return objectMapper.readValue(str, javaType);
    } catch (Exception e) {
      log.warn("Parse String to Object error", e);
      return string2Obj2(str, collectionClass, elementClasses);
    }
  }

  public static <T> T string2Obj2(String str, Class<?> collectionClass, Class<?>... elementClasses) {
    com.fasterxml.jackson.databind.JavaType javaType = mapper.getTypeFactory().constructParametricType(collectionClass, elementClasses);
    try {
      return mapper.readValue(str, javaType);
    } catch (Exception e) {
      log.warn("Parse String to Object error", e);
      return null;
    }
  }

}
