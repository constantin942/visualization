package com.aiit.skyflying.common.utils;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import java.text.ParseException;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.*;

/**
 * <B>方法名称：DateUtil</B>
 * <B>概要说明：时间工具类</B>
 *
 * @Author zm
 * @Date 2022年09月07日 14:09:59
 * @Param
 * @return
 **/
@Slf4j
public class DateUtil {

  private DateUtil(){}
  /**
   * milliseconds in a second.
   */
  public static final long SECOND = 1000;

  /**
   * milliseconds in a minute.
   */
  public static final long MINUTE = SECOND * 60;

  /**
   * milliseconds in a hour.
   */
  public static final long HOUR = MINUTE * 60;

  /**
   * yyyy-MM-dd
   */
  public static final String DEFAULT_PATTERN_WITH_HYPHEN = "yyyy-MM-dd";

  /**
   * <pre>
   * yyyy-MM-dd HH:mm:ss
   * </pre>
   */
  public static final String DATE_TIME_SHORT = "yyyy-MM-dd HH:mm:ss";

  public static final String FINAL_END_DATE_STR = "9999-12-30";

  private static final String YYYY_MM_DD1 = "yyyyMMdd";
  private static final String YYYY_MM_DD2 = "yyyy-MM-dd";

  public static String formatWithDateTimeShort(final Date date) {
    if (date == null) {
      return "";
    }
    return formatDate(date, DATE_TIME_SHORT);
  }

  public static String formatDate(final Date date, String format) {
    return new SimpleDateFormat(format).format(date);
  }

  public static Date parseDate(final String str, final String parsePatterns) {
    if (StringUtils.isBlank(str) || StringUtils.isBlank(parsePatterns)) {
      throw new IllegalArgumentException("Date and  Patterns must not be null");
    }
    SimpleDateFormat parser = new SimpleDateFormat(parsePatterns);
    try {
      return parser.parse(str);
    } catch (Exception e) {
      log.error("");
    }
    return null;
  }

  /**
   * 返回一个Date默认最大值
   *
   * @return
   */
  public static Date getFinalDate() {
    return parseDate(FINAL_END_DATE_STR, DEFAULT_PATTERN_WITH_HYPHEN);
  }


  /**
   * 返回两个时间间隔的小时数
   *
   * @param before 起始时间
   * @param end    终止时间
   * @return 小时数
   */
  public static long getNumberOfHoursBetween(final Date before, final Date end) {
    long millisec = end.getTime() - before.getTime() + 1;
    return millisec / (60 * 60 * 1000);
  }

  public static Date getNowDate() {
    return new Date();
  }

  /**
   * 获得当前日期
   *
   * @return
   */
  public static Date getNow() {
    Calendar cal = Calendar.getInstance();
    return cal.getTime();
  }

  /**
   * 日期转换为字符串 格式自定义
   *
   * @param date
   * @param f
   * @return
   */
  public static String dateStr(Date date, String f) {
    SimpleDateFormat format = new SimpleDateFormat(f);
    if (date != null) {
      return format.format(date);
    }
    return "";
  }

  /**
   * yyyy-MM-dd HH:mm:ss
   *
   * @param date
   * @return
   */
  public static String dateStr4(Date date) {
    return dateStr(date, DATE_TIME_SHORT);

  }

  /**
   * 将时间戳转换为Date
   *
   * @param times
   * @return
   */
  public static Date getDate(String times) {
    long time = Long.parseLong(times);
    return new Date(time * 1000);
  }

  /**
   * 将Date转换为时间戳
   *
   * @param date
   * @return
   */
  public static long getTime(Date date) {
    return date.getTime() / 1000;
  }

  /**
   * 默认的valueOf 方法，格式化 yyyy-mm-dd HH:mm:ss
   *
   * @param str
   * @return
   */
  public static Date valueOf(String str) {
    return valueOf(str, DATE_TIME_SHORT);
  }

  /**
   * 自定义format格式化字符串为date
   *
   * @param str           要格式化的字符串
   * @param dateFormatStr
   * @return
   */
  public static Date valueOf(String str, String dateFormatStr) {
    SimpleDateFormat formatter = new SimpleDateFormat(dateFormatStr);
    ParsePosition pos = new ParsePosition(0);
    return formatter.parse(str, pos);
  }

  /**
   * 获取当前时间-时间戳字符串
   *
   * @return
   */
  public static String getNowTimeStr() {
    return Long.toString(System.currentTimeMillis() / 1000);
  }

  public static Date getIntegralTime() {
    Calendar cal = Calendar.getInstance();
    cal.set(Calendar.HOUR_OF_DAY, 0);
    cal.set(Calendar.SECOND, 0);
    cal.set(Calendar.MINUTE, 0);
    cal.set(Calendar.MILLISECOND, 0);
    return cal.getTime();
  }

  public static Date getLastIntegralTime() {
    Calendar cal = Calendar.getInstance();
    cal.set(Calendar.HOUR_OF_DAY, 23);
    cal.set(Calendar.SECOND, 59);
    cal.set(Calendar.MINUTE, 59);
    cal.set(Calendar.MILLISECOND, 0);
    return cal.getTime();
  }


  /**
   * 获取指定时间天的结束时间
   *
   * @return
   */
  public static Date getDayEndTime(Date day) {
    Calendar cal = Calendar.getInstance();
    if (day != null) {
      cal.setTimeInMillis(day.getTime());
    }
    cal.set(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DATE), 23, 59, 59);
    return cal.getTime();
  }


  /**
   * 字符串转时间
   *
   * @param str
   * @param dateFormatStr
   * @return
   */
  public static Date strToDate(String str, String dateFormatStr) {
    SimpleDateFormat format = new SimpleDateFormat(dateFormatStr);
    Date date = null;
    try {
      date = format.parse(str);
    } catch (ParseException e) {
      e.printStackTrace();
    }
    return date;
  }

  /**
   * 获取昨天 开始时间
   *
   * @return
   */
  public static Date getYesterday() {
    Calendar cal = Calendar.getInstance();
    cal.add(Calendar.DATE, -1);
    cal.set(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DATE), 0, 0, 0);
    return cal.getTime();
  }

  public static final int REAL_FIRST_DAY = Calendar.SUNDAY;

  private static void setToRealFirstDay(Calendar calendar) {
    while (calendar.get(Calendar.DAY_OF_WEEK) != REAL_FIRST_DAY) {
      calendar.add(Calendar.DATE, -1);
    }
  }

  public static String getRealWeekEnddays() {
    Calendar calendar = Calendar.getInstance();
    setToRealFirstDay(calendar);
    calendar.add(Calendar.DATE, 6);
    Date date = DateUtil.getDayEndTime(calendar.getTime());
    SimpleDateFormat dateFormat = new SimpleDateFormat(DateTimeUtil.STANDARD_FORMAT);

    return dateFormat.format(date);
  }

  /**
   * 获取今天 开始时间
   *
   * @return
   */
  public static Date getToday() {
    Calendar cal = Calendar.getInstance();
    cal.add(Calendar.DATE, 0);
    cal.set(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DATE), 0, 0, 0);
    return cal.getTime();
  }


  public static int getCurrentHour() {
    Calendar cal = Calendar.getInstance();
    return cal.get(Calendar.HOUR_OF_DAY);
  }

  /**
   * 获取当前日期字符串
   *
   * @return
   */
  public static String getCurrentDateStr() {
    SimpleDateFormat sdf = new SimpleDateFormat(YYYY_MM_DD2);
    return sdf.format(DateUtil.getNow());
  }


  /**
   * 获取当前简单的年月日
   *
   * @return
   */
  public static String getCurrentSimpleDateStr() {
    SimpleDateFormat sdf = new SimpleDateFormat(YYYY_MM_DD1);
    return sdf.format(DateUtil.getNow());
  }

  /**
   * 根据指定的Format转化java.util.Date到String
   *
   * @param dt   java.util.Date instance
   * @param sFmt Date format , DATE_FORMAT_DATEONLY or DATE_FORMAT_DATETIME
   * @return
   * @history
   * @since 1.0
   */
  public static String toString(Date dt, String sFmt) {
    if (dt == null || sFmt == null || "".equals(sFmt)) {
      return "";
    }
    return toString(dt, new SimpleDateFormat(sFmt));
  }

  /**
   * 利用指定SimpleDateFormat instance转换java.util.Date到String
   *
   * @param dt        java.util.Date instance
   * @param formatter SimpleDateFormat Instance
   * @return
   * @history
   * @since 1.0
   */
  private static String toString(Date dt, SimpleDateFormat formatter) {
    String sRet = null;
    try {
      sRet = formatter.format(dt);
    } catch (Exception e) {
      sRet = null;
    }
    return sRet;
  }

}
