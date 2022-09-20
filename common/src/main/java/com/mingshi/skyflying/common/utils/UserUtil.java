package com.mingshi.skyflying.common.utils;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * @类描述：用户工具类
 */
@Slf4j
@Component
public class UserUtil {

  public String getPassword(String passwordSrc, String salt) {
    return DigestUtil.digestString(passwordSrc, salt);
  }

  public String getSalt() {
    byte[] saltBytes = DigestUtil.generateSalt(DigestUtil.DEFAULT_BYTES_SIZE);
    return DigestUtil.encodeHex(saltBytes);
  }
}
