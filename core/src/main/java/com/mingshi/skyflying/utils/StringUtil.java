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

package com.mingshi.skyflying.utils;

import java.security.MessageDigest;
import java.util.function.Consumer;

public final class StringUtil {
  public static boolean isEmpty(String str) {
    return str == null || str.length() == 0;
  }

  public static boolean isNotEmpty(String str) {
    return !isEmpty(str);
  }

  public static boolean isBlank(String str) {
    return str == null || isEmpty(str.trim());
  }

  public static boolean isNotBlank(String str) {
    return !isBlank(str);
  }

  public static void setIfPresent(String value, Consumer<String> setter) {
    if (isNotEmpty(value)) {
      setter.accept(value);
    }
  }

  /**
   * <B>方法名称：recombination</B>
   * <B>概要说明：大写统一转换成消息、去掉多余的空格</B>
   * @Author zm
   * @Date 2022年05月27日 15:05:42
   * @Param [msSql, opTime, msSchemaName, sqlType]
   * @return java.lang.String
   **/
  public static String recombination(String msSql, String opTime, String msSchemaName, String sqlType) {
    String strData = null;
    if(StringUtil.isBlank(opTime)){
      strData = (msSql + msSchemaName + sqlType).toLowerCase().trim();
    }else{
      strData = (msSql + opTime + msSchemaName + sqlType).toLowerCase().trim();
    }

    return strData;
  }

  public static String MD5(String key) {
    char hexDigits[] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};
    try {
      byte[] btInput = key.getBytes();// 获得MD5摘要算法的 MessageDigest 对象
      MessageDigest mdInst = MessageDigest.getInstance("MD5");// 使用指定的字节更新摘要
      mdInst.update(btInput);// 获得密文
      byte[] md = mdInst.digest();// 把密文转换成十六进制的字符串形式
      int j = md.length;
      char str[] = new char[j * 2];
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

  public static String join(final char delimiter, final String... strings) {
    if (strings.length == 0) {
      return null;
    }
    if (strings.length == 1) {
      return strings[0];
    }
    int length = strings.length - 1;
    for (final String s : strings) {
      if (s == null) {
        continue;
      }
      length += s.length();
    }
    final StringBuilder sb = new StringBuilder(length);
    if (strings[0] != null) {
      sb.append(strings[0]);
    }
    for (int i = 1; i < strings.length; ++i) {
      if (!isEmpty(strings[i])) {
        sb.append(delimiter).append(strings[i]);
      } else {
        sb.append(delimiter);
      }
    }
    return sb.toString();
  }

  public static boolean substringMatch(CharSequence str, int index, CharSequence substring) {
    if (index + substring.length() > str.length()) {
      return false;
    }
    for (int i = 0; i < substring.length(); i++) {
      if (str.charAt(index + i) != substring.charAt(i)) {
        return false;
      }
    }
    return true;
  }

  public static String cut(String str, int threshold) {
    if (isEmpty(str) || str.length() <= threshold) {
      return str;
    }
    return str.substring(0, threshold);
  }

  public static String trim(final String str, final char ch) {
    if (isEmpty(str)) {
      return null;
    }

    final char[] chars = str.toCharArray();

    int i = 0, j = chars.length - 1;
    // noinspection StatementWithEmptyBody
    for (; i < chars.length && chars[i] == ch; i++) {
    }
    // noinspection StatementWithEmptyBody
    for (; j > 0 && chars[j] == ch; j--) {
    }

    return new String(chars, i, j - i + 1);
  }
}
