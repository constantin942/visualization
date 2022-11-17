package com.mingshi.skyflying.common.utils;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.DecoderException;
import org.apache.commons.codec.binary.Hex;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.Validate;
import org.springframework.web.multipart.MultipartFile;

import java.io.*;
import java.security.GeneralSecurityException;
import java.security.MessageDigest;
import java.security.SecureRandom;

@Slf4j
public class DigestUtil {

  public static final String SHA1 = "SHA-1";
  public static final int DEFAULT_BYTES_SIZE = 8;
  public static final int DEFAULT_DIGEST_TIMES = 1024;

  /**
   * 对字符串进行给定次数和指定算法的散列(包含盐值)
   *
   * @return
   */
  public static String digestString(String password, String salt) {
    byte[] saltBytes = salt.getBytes();
    byte[] pwdBytes = digest(getStringByte(password), SHA1, saltBytes, DEFAULT_DIGEST_TIMES);
    return encodeHex(pwdBytes);
  }

  public static byte[] getStringByte(String str) {
    if (str != null && !"".equals(str.trim())) {
      try {
        return str.getBytes("UTF-8");
      } catch (UnsupportedEncodingException e) {
        return null;
      }
    } else {
      return null;
    }
  }

  /**
   * 生成随机salt的字节数组
   *
   * @param num
   * @return
   */
  public static byte[] generateSalt(int num) {
    Validate.isTrue(num > 0, "num argument must be a positive integer, larger than 0", num);

    byte[] bytes = new byte[num];
    SecureRandom random = new SecureRandom();
    random.nextBytes(bytes);
    return bytes;
  }

  private static byte[] digest(byte[] bytes, String algorithm, byte[] salt, int counts) {
    try {
      MessageDigest digest = MessageDigest.getInstance(algorithm);
      if (salt != null) {
        digest.update(salt);
      }

      byte[] result = digest.digest(bytes);

      for (int i = 1; i < counts; i++) {
        digest.reset();
        result = digest.digest(result);
      }
      return result;
    } catch (GeneralSecurityException e) {
      log.error("general security exception occurs, detail exception is ", e);
      return null;
    }
  }

  public static String encodeHex(byte[] data) {
    return Hex.encodeHexString(data);
  }

}
