package com.mingshi.skyflying.common.utils;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.client.RestTemplate;

/**
 * @author weilai
 * @email 352342845@qq.com
 * @date 2019-11-13 16:00
 */
@Slf4j
public class HttpUtil {

  static ObjectMapper mapper;

  static {
    mapper = new ObjectMapper();
    mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
  }

  public static String post(String url) {
    RestTemplate restTemplate = new RestTemplate();
    return restTemplate.postForObject(url, null, String.class);
  }

  public static String get(String url) {
    RestTemplate restTemplate = new RestTemplate();
    return restTemplate.getForObject(url, String.class);
  }

  public static String get(String url, Object object) {
    RestTemplate restTemplate = new RestTemplate();
    return restTemplate.getForObject(url, String.class);
  }

}
