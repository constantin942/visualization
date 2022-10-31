/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package com.mingshi.skyflying.common.utils;

import org.apache.commons.lang3.Validate;

import java.security.MessageDigest;
import java.util.Random;

public final class StringUtil {
  public static boolean isEmpty(String str) {
    return str == null || str.length() == 0;
  }

  public static boolean isBlank(String str) {
    return str == null || isEmpty(str.trim());
  }

  public static boolean isNotBlank(String str) {
    return !isBlank(str);
  }

  public static final boolean IS_OS_WINDOWS;
  private static final Random RANDOM = new Random();
  public static final String OS_NAME;

  static {
    IS_OS_WINDOWS = getOsMatchesName("Windows");
    OS_NAME = getSystemProperty("os.name");
  }

  public static long nextLong(long startInclusive, long endExclusive) {
    Validate.isTrue(endExclusive >= startInclusive, "Start value must be smaller or equal to end value.", new Object[0]);
    Validate.isTrue(startInclusive >= 0L, "Both range values must be non-negative.", new Object[0]);
    return startInclusive == endExclusive ? startInclusive : (long) nextDouble((double) startInclusive, (double) endExclusive);
  }

  public static double nextDouble(double startInclusive, double endInclusive) {
    Validate.isTrue(endInclusive >= startInclusive, "Start value must be smaller or equal to end value.", new Object[0]);
    Validate.isTrue(startInclusive >= 0.0D, "Both range values must be non-negative.", new Object[0]);
    return startInclusive == endInclusive ? startInclusive : startInclusive + (endInclusive - startInclusive) * RANDOM.nextDouble();
  }

  private static boolean getOsMatchesName(String osNamePrefix) {
    return isOsNameMatch(OS_NAME, osNamePrefix);
  }

  static boolean isOsNameMatch(String osName, String osNamePrefix) {
    return osName == null ? false : osName.startsWith(osNamePrefix);
  }

  private static String getSystemProperty(String property) {
    try {
      return System.getProperty(property);
    } catch (SecurityException var2) {
      return null;
    }
  }

  public static String getHostName() {
    return IS_OS_WINDOWS ? System.getenv("COMPUTERNAME") : System.getenv("HOSTNAME");
  }

  /**
   * <B>方法名称：recombination</B>
   * <B>概要说明：大写统一转换成消息、去掉多余的空格</B>
   *
   * @return java.lang.String
   * @Author zm
   * @Date 2022年05月27日 15:05:42
   * @Param [msSql, opTime, msSchemaName, sqlType]
   **/
  public static String recombination(String msSql, String opTime, String msSchemaName, String sqlType) {
    String strData = null;
    if (StringUtil.isBlank(opTime)) {
      strData = (msSql + msSchemaName + sqlType).toLowerCase().trim();
    } else {
      strData = (msSql + opTime + msSchemaName + sqlType).toLowerCase().trim();
    }

    return strData;
  }

  public static String mD5(String key) {
    char[] hexDigits = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};
    try {
      // 获得MD5摘要算法的 MessageDigest 对象
      byte[] btInput = key.getBytes();
      // 使用指定的字节更新摘要
      MessageDigest mdInst = MessageDigest.getInstance("MD5");
      // 获得密文
      mdInst.update(btInput);
      // 把密文转换成十六进制的字符串形式
      byte[] md = mdInst.digest();
      int j = md.length;
      char[] str = new char[j * 2];
      int k = 0;
      for (int i = 0; i < j; i++) {
        byte byte0 = md[i];
        str[k++] = hexDigits[byte0 >>> 4 & 0xf];
        str[k++] = hexDigits[byte0 & 0xf];
      }
      return new String(str);
    } catch (Exception e) {
      return null;
    }
  }
}
