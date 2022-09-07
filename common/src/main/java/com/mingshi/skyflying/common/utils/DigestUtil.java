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
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.Locale;


/**
 * @author 49090 2017年12月26日上午11:46:35
 * 本内容仅限于北大信研院安全工程与云计算实验室内部传阅，禁止外泄以及用于其他的商业目的
 * @类描述：
 */
@Slf4j
public class DigestUtil {

  public static final String SHA1 = "SHA-1";
  public static final int DEFAULT_BUFFER_LENGTH = 8 * 1024;
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
   * 获取16位MD5码
   *
   * @param md5Str
   * @return
   */
  public static String MD5_16(String md5Str) {
    String md5 = MD5(md5Str);
    return StringUtils.substring(md5, 8, 24);
  }

  /**
   * 32位Md5码
   *
   * @param md5Str
   * @return
   */
  public static String MD5(String md5Str) {
    if (StringUtils.isBlank(md5Str)) {
      return null;
    }
    return DigestUtils.md5Hex(md5Str);
  }

  public static String getSha1Code(MultipartFile fileObj) throws IOException {
    if (null == fileObj) {
      return StringUtils.EMPTY;
    }
    InputStream ins = null;
    try {
      ins = fileObj.getInputStream();
      return DigestUtils.sha1Hex(ins);
    } catch (Exception e) {
      return StringUtils.EMPTY;
    } finally {
      if (null != ins) {
        ins.close();
      }
    }
  }

  public static String getMd5(String pathname) throws IOException {
    if (StringUtils.isBlank(pathname)) {
      return StringUtils.EMPTY;
    }
    InputStream ins = null;
    try {
      ins = new FileInputStream(pathname);
      return DigestUtils.md5Hex(ins);
    } catch (Exception e) {
      return StringUtils.EMPTY;
    } finally {
      if (null != ins) {
        ins.close();
      }
    }
  }

  public static String getSha1Code(String pathname) throws IOException {
    if (StringUtils.isBlank(pathname)) {
      return StringUtils.EMPTY;
    }
    InputStream ins = null;
    try {
      ins = new FileInputStream(pathname);
      return getSha1Code(ins);
    } catch (Exception e) {
      return StringUtils.EMPTY;
    } finally {
      if (null != ins) {
        ins.close();
      }
    }
  }

  public static String getMd5(File file) throws IOException {
    if (null == file) {
      return StringUtils.EMPTY;
    }
    InputStream ins = null;
    try {
      ins = new FileInputStream(file);
      return getMd5(ins);
    } catch (Exception e) {
      return StringUtils.EMPTY;
    } finally {
      if (null != ins) {
        ins.close();
      }
    }
  }

  public static String getSha1Code(File file) throws IOException {
    if (null == file) {
      return StringUtils.EMPTY;
    }
    InputStream ins = null;
    try {
      ins = new FileInputStream(file);
      return getSha1Code(ins);
    } catch (Exception e) {
      return StringUtils.EMPTY;
    } finally {
      if (null != ins) {
        ins.close();
      }
    }
  }

  public static String getSha1Code(byte[] byteArray) throws IOException {
    if (null == byteArray || byteArray.length < 1) {
      return StringUtils.EMPTY;
    }
    return DigestUtils.sha1Hex(byteArray);
  }

  /**
   * get Sha1 Code with inputStream
   *
   * @param iStream
   * @return
   * @throws IOException
   */
  public static String getSha1Code(InputStream iStream) throws IOException {
    if (null == iStream) {
      return StringUtils.EMPTY;
    }
    return DigestUtils.sha1Hex(iStream);
  }

  /**
   * Generate Sha1 Code with inputStream
   *
   * @param iStream
   * @return
   * @throws IOException
   */
  public static String getMd5(InputStream iStream) throws IOException {
    if (null == iStream) {
      return StringUtils.EMPTY;
    }
    return DigestUtils.md5Hex(iStream);
  }

  /**
   * 对字符串进行给定算法散列(无盐值)
   *
   * @param bytes
   * @param algorithm
   * @return
   */
  public static byte[] digestString(byte[] bytes, String algorithm) {
    return digest(bytes, algorithm, null, 1);
  }

  /**
   * 对字符串进行给定算法散列(包含盐值)
   *
   * @param bytes
   * @param algorithm
   * @return
   */
  public static byte[] digestString(byte[] bytes, byte[] salt, String algorithm) {
    return digest(bytes, algorithm, salt, 1);
  }

  /**
   * 对字符串进行给定次数和指定算法的散列(包含盐值)
   *
   * @param bytes
   * @param algorithm
   * @return
   */
  public static byte[] digestString(byte[] bytes, byte[] salt, int counts, String algorithm) {
    return digest(bytes, algorithm, salt, counts);
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

  /**
   * byte[]字节组转为字符串
   *
   * @param b
   * @return
   */
  private static String byte2hex(byte[] b) {
    String hs = "";
    String stmp = "";
    for (int n = 0; n < b.length; n++) {
      stmp = (Integer.toHexString(b[n] & 0XFF));
      if (stmp.length() == 1) {
        hs = hs + "0" + stmp;
      } else {
        hs = hs + stmp;
      }
      if (n < b.length - 1) {
        hs = hs + ":";
      }
    }
    return hs.toUpperCase(Locale.ENGLISH);
  }

  /**
   * decode String to bytes[]
   *
   * @param src
   * @return
   */
  public static byte[] decodeHex(String src) {
    if (StringUtils.isBlank(src)) {
      return null;
    }
    try {
      return Hex.decodeHex(src.toCharArray());
    } catch (DecoderException e) {
      log.error("decode hex src failed, src vlaue is " + src, e);
    }
    return null;
  }

  public static String encodeHex(byte[] data) {
    return Hex.encodeHexString(data);
  }

  public static String sha256Hash(String strText) {
    // 返回值
    String strResult = null;

    // 是否是有效字符串
    if (strText != null && strText.length() > 0) {
      try {
        // SHA 加密开始
        // 创建加密对象 并传入加密类型
        MessageDigest messageDigest = MessageDigest.getInstance("SHA-256");
        // 传入要加密的字符串
        messageDigest.update(strText.getBytes());
        // 得到 byte 类型结果
        byte byteBuffer[] = messageDigest.digest();

        // 将 byte 转换为 string
        StringBuffer strHexString = new StringBuffer();
        // 遍歷 byte buffer
        for (int i = 0; i < byteBuffer.length; i++) {
          String hex = Integer.toHexString(0xff & byteBuffer[i]);
          if (hex.length() == 1) {
            strHexString.append('0');
          }
          strHexString.append(hex);
        }
        // 得到返回結果
        strResult = strHexString.toString();
      } catch (NoSuchAlgorithmException e) {
        e.printStackTrace();
      }
    }
    return strResult;
  }


}
