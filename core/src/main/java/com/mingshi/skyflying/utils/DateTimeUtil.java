package com.mingshi.skyflying.utils;

import com.mingshi.skyflying.enums.ConstantsCode;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.joda.time.DateTime;
import org.joda.time.Days;
import org.joda.time.Seconds;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.time.*;
import java.time.temporal.ChronoUnit;
import java.util.*;

/**
 * Created by geely
 */
@Slf4j
public class DateTimeUtil {

  public static final String STANDARD_FORMAT = "yyyy-MM-dd HH:mm:ss";
  public static final String STANDARD_FORMAT_T = "yyyy-MM-dd'T'HH:mm:ss";
  public static final String STANDARD_FORMAT_yyyyMMdd = "yyyy-MM-dd";
  public static final String STANDARD_FORMAT_yyyyMMddHHmmssSSS = "yyyyMMddHHmmssSSS";
  public static final String STANDARD_FORMAT_yyyy_MM_dd_HHmmssSSSSSS = "yyyy-MM-dd HH:mm:ss.SSSSSS";
  public static final String STANDARD_FORMAT_yyyyMMddHHmmss = "yyyyMMddHHmmss";
  public static final String STANDARD_FORMAT_HHMMSS = "HH:mm:ss";
  public static final String STANDARD_FORMAT_HHMM = "HH:mm";
  public static final String STANDARD_FORMAT_HH = "HH";
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
   * milliseconds in a day.
   */
  public static final long DAY = 24 * HOUR;

  /**
   * yyyyMMdd
   */
  public static final String DEFAULT_PATTERN = "yyyyMMdd";

  /**
   * yyyy-MM-dd
   */
  public static final String DEFAULT_PATTERN_WITH_HYPHEN = "yyyy-MM-dd";

  /**
   * yyyy-MM
   */
  public static final String MONTH_PATTERN = "yyyy-MM";

  /**
   * yyyy.MM.dd
   */
  public static final String DEFAULT_PATTERN_WITH_DOT = "yyyy.MM.dd";

  /**
   * yyyy年MM月dd
   */
  public static final String DEFAULT_CHINESE_PATTERN = "yyyy年MM月dd";

  /**
   * yyyyMMddHH
   */
  public static final String HOUR_PATTERN = "yyyyMMddHH";

  /**
   * yyyyMMddHHmmss
   */
  public static final String FULL_PATTERN = "yyyyMMddHHmmss";


  /**
   * MM.dd HH:mm
   */
  public static final String FULL_MATCH_PATTERN = "MM.dd HH:mm";

  /**
   * HH:mm
   */
  public static final String SHORT_MATCH_PATTERN = "HH:mm";

  /**
   * yyyy-MM-dd HH:mm
   */
  public static final String DATE_TIME_MINUTE = "yyyy-MM-dd HH:mm";

  /**
   * <pre>
   * yyyy-MM-dd HH:mm:ss
   * </pre>
   */
  public static final String DATE_TIME_SHORT = "yyyy-MM-dd HH:mm:ss";

  /**
   * <pre>
   * yyyy-MM-dd HH:mm:ss.SSS
   * </pre>
   */
  public static final String DATE_TIME_FULL = "yyyy-MM-dd HH:mm:ss.SSS";

  public static final String DATE_TIME_FULL_ALL = "yyyyMMddHHmmssSSS";

  public static final String FINAL_END_DATE_STR = "9999-12-30";


  /**
   * 格式 ：yyyy-MM-dd HH:mm:ss
   */
  public static final String DATEFORMAT_STR_001 = "yyyy-MM-dd HH:mm:ss";

  public static final String DATEFORMAT_STR_006 = "yyyy/MM/dd HH:mm:ss";
  /**
   * 格式 ：yyyy-MM-dd
   */
  public static final String DATEFORMAT_STR_002 = "yyyy-MM-dd";
  /**
   * 格式 ：MM-dd
   */
  public static final String DATEFORMAT_STR_003 = "MM-dd";
  /**
   * 格式 ：HH:mm:ss
   */
  public static final String DATEFORMAT_STR_004 = "HH:mm:ss";
  /**
   * 格式 ：yyyy-MM
   */
  public static final String DATEFORMAT_STR_005 = "yyyy-MM";

  /**
   * 格式 ：yyyyMMddHHmmss
   */
  public static final String DATEFORMAT_STR_011 = "yyyyMMddHHmmss";
  /**
   * 格式 ：yyyyMMdd
   */
  public static final String DATEFORMAT_STR_012 = "yyyyMMdd";

  /**
   * 格式 ：yyyy年MM月dd日 HH时mm分ss秒
   */
  public static final String DATEFORMAT_STR_021 = "yyyy年MM月dd日 HH时mm分ss秒";
  /**
   * 格式 ：yyyy年MM月dd日
   */
  public static final String DATEFORMAT_STR_022 = "yyyy年MM月dd日";
  /**
   * 格式 ：MM月dd日 hh:mm
   */
  public static final String DATEFORMAT_STR_023 = "MM月dd日 hh:mm";

  /**
   * <B>方法名称：instantToString</B>
   * <B>概要说明：将Instant类型的时间转换成String类型的时间，使用默认的时间格式 </B>
   *
   * @return java.lang.String
   * @Author zm
   * @Date 2022年07月04日 09:07:50
   * @Param []
   **/
  public static String instantToString1(Instant instant) {
    java.time.format.DateTimeFormatter formatter = java.time.format.DateTimeFormatter.ofPattern(STANDARD_FORMAT).withZone(ZoneId.systemDefault()).withLocale(Locale.CHINA);
    return formatter.format(instant);
  }

  /**
   * <B>方法名称：instantToString</B>
   * <B>概要说明：将Instant类型的时间转换成String类型的时间，使用指定的时间格式 </B>
   *
   * @return java.lang.String
   * @Author zm
   * @Date 2022年07月04日 09:07:50
   * @Param []
   **/
  public static String instantToString2(Instant instant, String dateFormat) {
    java.time.format.DateTimeFormatter formatter = java.time.format.DateTimeFormatter.ofPattern(dateFormat).withZone(ZoneId.systemDefault()).withLocale(Locale.CHINA);
    return formatter.format(instant);
  }

  /**
   * <B>方法名称：stringToInstant</B>
   * <B>概要说明：将String类型的时间转换成Instant类型的时间，使用默认的时间格式 </B>
   *
   * @return java.time.Instant
   * @Author zm
   * @Date 2022年07月04日 09:07:50
   * @Param []
   **/
  public static Instant stringToInstant1(String date) {
    // 将String类型的时间转换成Instant类型的时间；2022-07-04 09:00:46
    Instant instant = LocalDateTime.parse(date, java.time.format.DateTimeFormatter.ofPattern(STANDARD_FORMAT)
      .withZone(ZoneId.systemDefault()).withLocale(Locale.CHINA)).toInstant(ZoneOffset.UTC);
    return instant;
  }

  /**
   * <B>方法名称：stringToInstant</B>
   * <B>概要说明：将String类型的时间转换成Instant类型的时间，使用指定的时间格式 </B>
   *
   * @return java.time.Instant
   * @Author zm
   * @Date 2022年07月04日 09:07:50
   * @Param []
   **/
  public static Instant stringToInstant2(String date, String dateFormat) {
    // 将String类型的时间转换成Instant类型的时间；2022-07-04 09:00:46
    Instant instant = LocalDateTime.parse(date, java.time.format.DateTimeFormatter.ofPattern(dateFormat)
      .withZone(ZoneId.systemDefault()).withLocale(Locale.CHINA)).toInstant(ZoneOffset.UTC);
    return instant;
  }

  /**
   * <B>方法名称：longToDate</B>
   * <B>概要说明：长整形转换成字符串类型的时间</B>
   *
   * @return java.lang.String
   * @Author zm
   * @Date 2022年04月20日 16:04:43
   * @Param [longTime]
   **/
  public static String longToDate(long longTime) {
    Date date = new Date(longTime);
    SimpleDateFormat simpleDateFormat = new SimpleDateFormat(DATEFORMAT_STR_001);
    return simpleDateFormat.format(date);
  }

  /**
   * @return long
   * @Author zhaoming
   * @Description 获取两个时间差的毫秒数
   * @Date 14:51 2020/7/14
   * @Param [instStart]
   **/
  public static long getTimeMillis(Instant instStart) {
    Instant instEnd = Instant.now();
    return Duration.between(instStart, instEnd).toMillis();
  }

  /**
   * @return long
   * @Author zhaoming
   * @Description 获取两个时间差的纳秒数
   * @Date 14:51 2020/7/14
   * @Param [instStart]
   **/
  public static long getTimeNanos(Instant instStart) {
    Instant instEnd = Instant.now();
    return Duration.between(instStart, instEnd).toNanos();
  }

  /**
   * @return long
   * @Author zhaoming
   * @Description 获取两个时间差的秒数
   * @Date 14:51 2020/7/14
   * @Param [instStart]
   **/
  public static long getTimeSeconds(Instant instStart) {
    Instant instEnd = Instant.now();
    return Duration.between(instStart, instEnd).getSeconds();
  }

  // 计算两个时间的差
  public static double timeDifference(String time1, String time2) {
    double diff = -1;
    String formatIn = "yyyy-MM-dd HH:mm:ss.SSSSSS";

    try {
      Calendar c1 = Calendar.getInstance();
      c1.setTime(new SimpleDateFormat(formatIn).parse(time1));
      log.info("时间转化后的毫秒数为：" + c1.getTimeInMillis());

      Calendar c2 = Calendar.getInstance();
      c2.setTime(new SimpleDateFormat(formatIn).parse(time2));
      log.info("时间转化后的毫秒数为：" + c2.getTimeInMillis());
      diff = c1.getTimeInMillis() - c2.getTimeInMillis();
    } catch (ParseException e) {
      e.printStackTrace();
    }

    return Math.abs(diff);
  }

  //原始时间
  public static final Date INIT_DATE = new Date(0L);

  public static String formatDate(final Date date) {
    if (null == date) {
      return "";
    }
    return formatDate(date, DEFAULT_PATTERN);
  }

  public static String formatWithDATE_TIME_FULL(final Date date) {
    if (date == null) {
      return "";
    }
    return formatDate(date, DATE_TIME_FULL);
  }

  /**
   * 格式化日期
   *
   * @param date
   * @return
   */
  public static Date formatDateToYYYYMMdd(Date date) {
    if (date == null) {
      return null;
    }
    SimpleDateFormat sdf = new SimpleDateFormat(DEFAULT_PATTERN_WITH_HYPHEN);
    try {
      date = sdf.parse(sdf.format(date));
      return date;
    } catch (ParseException e) {
      return null;
    }

  }

  public static String formatWithDateTimeShort(final Date date) {
    if (date == null) {
      return "";
    }
    return formatDate(date, DATE_TIME_SHORT);
  }

  public static String formatWithDateTimeFullAll(final Date date) {
    if (date == null) {
      return "";
    }
    return formatDate(date, DATE_TIME_FULL_ALL);
  }

  public static String formatWithDateTimeHyphen(final Date date) {
    if (date == null) {
      return "";
    }
    return formatDate(date, DEFAULT_PATTERN_WITH_HYPHEN);
  }

  public static String formatWithDateTimeFullPattern(final Date date) {
    if (date == null) {
      return "";
    }
    return formatDate(date, DATE_TIME_SHORT);
  }

  public static String formatDate(final Date date, String format) {
    return new SimpleDateFormat(format).format(date);
  }

  public static String formatWithDateTimeHyphenAddDays(final Date date, int days) {
    if (date == null) {
      return "";
    }
    return formatDate(addDays(date, days), DEFAULT_PATTERN_WITH_HYPHEN);
  }

  /**
   * Add specified number of days to the given date.
   *
   * @param date date
   * @param days Int number of days to add
   * @return revised date
   */
  public static Date addDays(final Date date, int days) {
    Calendar cal = GregorianCalendar.getInstance();
    cal.setTime(date);
    cal.add(Calendar.DAY_OF_MONTH, days);

    return new Date(cal.getTime().getTime());
  }


  public static Date addHoures(Date date, int hour) {
    Calendar cal = Calendar.getInstance();
    cal.setTime(date);
    cal.add(Calendar.HOUR, hour);
    return cal.getTime();
  }

  public static Date addMins(final Date date, int mins) {
    Calendar cal = GregorianCalendar.getInstance();
    cal.setTime(date);
    cal.add(Calendar.MINUTE, mins);

    return new Date(cal.getTime().getTime());
  }

  public static Date addSeconds(final Date date, int seconds) {
    Calendar cal = GregorianCalendar.getInstance();
    cal.setTime(date);
    cal.add(Calendar.SECOND, seconds);

    return new Date(cal.getTime().getTime());
  }


  public static Date parseDate(final String str, final String parsePatterns) {
    if (StringUtils.isBlank(str) || StringUtils.isBlank(parsePatterns)) {
      throw new IllegalArgumentException("Date and Patterns must not be null");
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
   * 转换long类型到时,分,秒,毫秒的格式.
   *
   * @param time long type
   * @return
   */
  public static String convert(long time) {
    long ms = time % 1000;
    time /= 1000;

    int h = Integer.valueOf("" + (time / 3600));
    int m = Integer.valueOf("" + ((time - h * 3600) / 60));
    int s = Integer.valueOf("" + (time - h * 3600 - m * 60));

    return h + "小时(H)" + m + "分(M)" + s + "秒(S)" + ms + "毫秒(MS)";
  }

  /**
   * judge the srcDate is between startDate and endDate
   *
   * @param srcDate
   * @param startDate
   * @param endDate
   * @return
   */
  public static boolean isBetweenDateRange(final Date srcDate, final Date startDate, final Date endDate) {
    if (srcDate != null && startDate != null && endDate != null) {
      return srcDate.getTime() >= startDate.getTime() && srcDate.getTime() <= endDate.getTime();
    }
    return false;
  }

  /**
   * 获取指定的时间
   *
   * @param dayOffSet
   * @param hourOffSet
   * @param minuteOffSet
   * @param secondsOffSet
   * @return
   */
  public static Date getCertainDate(int dayOffSet, int hourOffSet, int minuteOffSet, int secondsOffSet) {
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, dayOffSet);
    calendar.set(Calendar.HOUR, hourOffSet);
    calendar.set(Calendar.MINUTE, minuteOffSet);
    calendar.set(Calendar.SECOND, secondsOffSet);
    return calendar.getTime();
  }

  public static Date getCentainOffDate(Date date, int dayOffSet) {
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(date);
    calendar.add(Calendar.DATE, dayOffSet);
    return calendar.getTime();
  }

  /**
   * <B>方法名称：getSecondByDate</B>
   * <B>概要说明：根据字符串类型的时间获取毫秒级时间差</B>
   *
   * @return long
   * @Author zm
   * @Date 2022年07月08日 09:07:14
   * @Param [stringDate]
   **/
  public static long getSecondByDate(String stringDate) {
    long timeFrom = DateTimeUtil.strToDate(stringDate).getTime();
    long timeEnd = System.currentTimeMillis();
    long interval = Math.abs((timeEnd - timeFrom) / 1000);
    return interval;
  }

  /**
   * <B>方法名称：getMilliSecondByDate</B>
   * <B>概要说明：根据字符串类型的时间获取秒级时间差</B>
   *
   * @return long
   * @Author zm
   * @Date 2022年07月08日 09:07:14
   * @Param [stringDate]
   **/
  public static long getMilliSecondByDate(String stringDate) {
    long timeFrom = DateTimeUtil.strToDate(stringDate).getTime();
    long timeEnd = System.currentTimeMillis();
    long interval = Math.abs((timeEnd - timeFrom));
    return interval;
  }

  /**
   * <B>方法名称：getDateList</B>
   * <B>概要说明：根据初识开始时间和初识结束时间，输出中间的每一天的时间</B>
   * @Author zm
   * @Date 2022年07月19日 17:07:41
   * @Param [startTime, endTime]
   * @return java.util.List<java.lang.String>
   **/
  public static List<String> getDateList(String startTime, String endTime) {
    List<String> DateList = new LinkedList<>();
    try {
      Calendar startTimeCalendar = Calendar.getInstance();
      Calendar endTimeCalendar = Calendar.getInstance();

      //创建SimpleDateFormat对象实例并定义好转换格式
      SimpleDateFormat sdf = new SimpleDateFormat(DateTimeUtil.STANDARD_FORMAT);

      Date startTimeDate = DateTimeUtil.strToDate(startTime);
      Date endTimeDate = DateTimeUtil.strToDate(endTime);
      startTimeCalendar.setTime(startTimeDate);
      endTimeCalendar.setTime(endTimeDate);

      while (startTimeCalendar.compareTo(endTimeCalendar) < 1) {
        String startTimeStr = sdf.format(startTimeCalendar.getTime());
        DateList.add(startTimeStr);
        startTimeCalendar.add(Calendar.DATE, 1);
      }

      if (startTimeCalendar.before(endTimeCalendar)) {
        String endTimeStr = sdf.format(startTimeCalendar.getTime());
        DateList.add(endTimeStr);
      }
    } catch (Exception e) {
      log.error("# DateTimeUtil.getDateList() # 转换时间时，出现了异常。", e);
    }
    return DateList;
  }


  public void LeaveSignIn() {
    try {
      SimpleDateFormat df = new SimpleDateFormat("HH:mm");//设置日期格式
      Date nowTime = df.parse(df.format(new Date()));
      // 规定白天时间为早上6点到晚上10点；2022-06-07 15:36:10
      Date amBeginTime = df.parse("06:00");
      Date pmEndTime = df.parse("22:00");
      //调用判断方法是否在规定时间段内
      boolean isTime = timeCalendar(nowTime, amBeginTime, pmEndTime);

      if (isTime) {
        //处于规定的时间段内
        System.out.println(isTime);
      } else {
        //不处于规定的时间段内
        System.out.println(isTime);
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * <B>方法名称：timeCalendar</B>
   * <B>概要说明：判断是否在规定的时间内签到 nowTime 当前时间 beginTime规定开始时间 endTime规定结束时间</B>
   *
   * @return boolean
   * @Author zm
   * @Date 2022年06月07日 15:06:15
   * @Param [nowTime, amBeginTime, amEndTime, pmBeginTime, pmEndTime]
   **/
  public static boolean timeCalendar(Date nowTime, Date amBeginTime, Date amEndTime) {
    //设置当前时间
    Calendar date = Calendar.getInstance();
    date.setTime(nowTime);
    //设置开始时间
    Calendar amBegin = Calendar.getInstance();
    amBegin.setTime(amBeginTime);//上午开始时间
    //设置结束时间
    Calendar amEnd = Calendar.getInstance();
    amEnd.setTime(amEndTime);//上午结束时间
    //处于开始时间之后，和结束时间之前的判断
    if ((date.after(amBegin) && date.before(amEnd))) {
      return true;
    } else {
      return false;
    }
  }

  /**
   * <B>方法名称：judgmentTime</B>
   * <B>概要说明：判断时间是上午、中午、下午、凌晨、晚上</B>
   *
   * @return java.lang.String
   * @Author zm
   * @Date 2022年06月07日 15:06:18
   * @Param []
   **/
  public static String judgmentTime(Date date) {
    SimpleDateFormat df = new SimpleDateFormat(STANDARD_FORMAT_HH);
    String str = df.format(date);
    int a = Integer.parseInt(str);
    if ((a >= 0 && a <= 6) || (a > 18 && a <= 24)) {
      // System.out.println("晚上");
      return ConstantsCode.USER_PORTRAIT_NIGHT.getCode();
    }
    if (a > 6 && a <= 12) {
      // System.out.println("上午");
      return ConstantsCode.USER_PORTRAIT_FORENOON.getCode();
    }
    if (a > 12 && a <= 18) {
      // System.out.println("下午");
      return ConstantsCode.USER_PORTRAIT_AFTERNOON.getCode();
    }
    // if (a >= 0 && a <= 6) {
    //   // System.out.println("凌晨");
    //   return "beforeDawn";
    // }
    // if (a > 6 && a <= 12) {
    //   // System.out.println("上午");
    //   return "forenoon";
    // }
    // if (a > 12 && a <= 13) {
    //   // System.out.println("中午");
    //   return "noon";
    // }
    // if (a > 13 && a <= 18) {
    //   // System.out.println("下午");
    //   return "afternoon";
    // }
    // if (a > 18 && a <= 24) {
    //   // System.out.println("晚上");
    //   return "night";
    // }
    return null;
  }

  /**
   * Get start of date.
   *
   * @param date Date
   * @return Date Date
   */
  public static Date getStartOfDate(final Date date) {
    Calendar cal = GregorianCalendar.getInstance();
    cal.setTime(date);

    // cal.set(Calendar.HOUR_OF_DAY, 0);
    cal.set(Calendar.MINUTE, 0);
    cal.set(Calendar.SECOND, 0);
    cal.set(Calendar.MILLISECOND, 0);

    return new Date(cal.getTime().getTime());
  }

  /**
   * Get date one day before specified one.
   *
   * @param date1 test date
   * @param date2 date when
   * @return true if date1 is before date2
   */
  public static boolean beforeDay(final Date date1, final Date date2) {
    return getStartOfDate(date1).before(getStartOfDate(date2));
  }

  /**
   * Get date one day after specified one.
   *
   * @param date1 Date 1
   * @param date2 Date 2
   * @return true if date1 after date2
   */
  public static boolean afterDay(final Date date1, final Date date2) {
    return date1.after(date2);
//        return getStartOfDate(date1).after(getStartOfDate(date2));
  }

  /**
   * 返回当前日
   *
   * @return [dd]
   */

  public static String getDay(Date date) {
    SimpleDateFormat formatter = new SimpleDateFormat("dd");
    String pid = formatter.format(date);
    return pid;
  }

  /**
   * 返回当前月份,如果date为null则返回当前月份
   *
   * @return [MM]
   */

  public static String getMonth(Date date) {
    if (date == null) {
      date = new Date();
    }
    SimpleDateFormat formatter = new SimpleDateFormat("MM");
    String pid = formatter.format(date);
    return pid;
  }

  public static Integer getCurrentMonth(Date date) {
    if (date == null) {
      date = new Date();
    }
    SimpleDateFormat formatter = new SimpleDateFormat("MM");
    String pid = formatter.format(date);
    return Integer.valueOf(pid);
  }

  /**
   * 返回当前年份,如果date为null则返回当前年份
   *
   * @return [MM]
   */

  public static String getYear(Date date) {
    if (date == null) {
      date = new Date();
    }
    SimpleDateFormat formatter = new SimpleDateFormat("yyyy");
    String pid = formatter.format(date);
    return pid;
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
   * <<<<<<< HEAD
   * Return the end of the month based on the date passed as input parameter.
   *
   * @param date Date
   * @return Date endOfMonth
   */
  public static Date getEndOfMonth(final Date date) {
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(date);

    calendar.set(Calendar.MONTH, calendar.get(Calendar.MONTH) + 1);
    calendar.set(Calendar.DATE, 0);

    calendar.set(Calendar.HOUR_OF_DAY, 12);
    calendar.set(Calendar.MINUTE, 0);
    calendar.set(Calendar.SECOND, 0);
    calendar.set(Calendar.MILLISECOND, 0);
    return new Date(calendar.getTimeInMillis());
  }

  /**
   * Get first day of month.
   *
   * @param date Date
   * @return Date
   */
  public static Date getFirstOfMonth(final Date date) {
    Date lastMonth = addMonths(date, -1);
    lastMonth = getEndOfMonth(lastMonth);
    return addDays(lastMonth, 1);
  }

  /**
   * Add specified number of months to the date given.
   *
   * @param date   Date
   * @param months Int number of months to add
   * @return Date
   */
  public static Date addMonths(Date date, int months) {
    Calendar cal = Calendar.getInstance();
    cal.setTime(date);
    cal.add(Calendar.MONTH, months);
    return cal.getTime();
  }

  /**
   * 计算2个日前直接相差的天数
   *
   * @param cal1
   * @param cal2
   * @return
   */
  public static long getNumberOfDaysBetween(Calendar cal1, Calendar cal2) {
    cal1.clear(Calendar.MILLISECOND);
    cal1.clear(Calendar.SECOND);
    cal1.clear(Calendar.MINUTE);
    cal1.clear(Calendar.HOUR_OF_DAY);

    cal2.clear(Calendar.MILLISECOND);
    cal2.clear(Calendar.SECOND);
    cal2.clear(Calendar.MINUTE);
    cal2.clear(Calendar.HOUR_OF_DAY);

    long elapsed = cal2.getTime().getTime() - cal1.getTime().getTime();
    return elapsed / DAY;
  }

  public static long getNumberOfDaysBetweenDates(final Date before, final Date end) {
    Calendar cal1 = Calendar.getInstance();
    cal1.setTime(before);
    Calendar cal2 = Calendar.getInstance();
    cal2.setTime(end);
    return getNumberOfDaysBetween(cal1, cal2);
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

  /**
   * 返回两个时间间隔的分钟数
   *
   * @param before 起始时间
   * @param end    终止时间
   * @return 分钟数
   */
  public static long getNumberOfMinuteBetween(final Date before, final Date end) {
    long millisec = end.getTime() - before.getTime();
    return millisec / (60 * 1000);
  }

  public static int getNumberOfMonthsBetween(final Date before, final Date end) {
    Calendar cal1 = Calendar.getInstance();
    cal1.setTime(before);
    Calendar cal2 = Calendar.getInstance();
    cal2.setTime(end);
    return (cal2.get(Calendar.YEAR) - cal1.get(Calendar.YEAR)) * 12
      + (cal2.get(Calendar.MONTH) - cal1.get(Calendar.MONTH));
  }

  public static int getNumberOfSecondsBetween(final double end, final double start) {
    if ((end == 0) || (start == 0)) {
      return -1;
    }

    return (int) (Math.abs(end - start) / SECOND);
  }

  public static Date getNowDate() {
    return new Date();
  }

  /**
   * get date time as "yyyyMMddhhmmss"
   *
   * @return the current date with time component
   */
  public static String getNowYearMonthDay() {
    return formatDate(new Date(), DEFAULT_PATTERN);
  }

  /**
   * get date time as "yyyyMMddhhmmss"
   *
   * @return the current date with time component
   */
  public static String getNowYearMonthDay(Date date) {
    return formatDate(date, DEFAULT_PATTERN);
  }

  public static String getDateTimeFullAll(Date date) {
    if (date == null) {
      return "";
    }
    try {
      return formatDate(date, DATE_TIME_FULL_ALL);
    } catch (Exception e) {
      return "";
    }
  }

  /**
   * 根据开始时间和结束时间返回时间段内的时间集合
   *
   * @param beginDate
   * @param endDate
   * @return
   */
  public static List<Date> getDatesBetweenTwoDate(Date beginDate, Date endDate) {
    List<Date> lDate = new ArrayList<Date>();
    lDate.add(beginDate);// 把开始时间加入集合
    Calendar cal = Calendar.getInstance();
    cal.setTime(beginDate);
    boolean bContinue = true;
    while (bContinue) {
      cal.add(Calendar.DAY_OF_MONTH, 1);
      if (endDate.after(cal.getTime())) {
        lDate.add(cal.getTime());
      } else {
        break;
      }
    }
    lDate.add(endDate);// 把结束时间加入集合
    return lDate;
  }

  /**
   * 一天的结束时间，【注：只精确到毫秒】
   *
   * @param date
   * @return
   */
  public static Date getEndOfDate(final Date date) {
    Calendar cal = GregorianCalendar.getInstance();
    cal.setTime(date);

    cal.set(Calendar.HOUR_OF_DAY, 23);
    cal.set(Calendar.MINUTE, 59);
    cal.set(Calendar.SECOND, 59);
    cal.set(Calendar.MILLISECOND, 999);

    return new Date(cal.getTime().getTime());
  }

  /**
   * 一天的结束时间，【注：只精确到秒】
   *
   * @param date
   * @return
   */
  public static Date getEndOfDatePrecisionSecond(final Date date) {
    Calendar cal = GregorianCalendar.getInstance();
    cal.setTime(date);

    cal.set(Calendar.HOUR_OF_DAY, 23);
    cal.set(Calendar.MINUTE, 59);
    cal.set(Calendar.SECOND, 59);
    cal.set(Calendar.MILLISECOND, 000);

    return new Date(cal.getTime().getTime());
  }

  /**
   * 一天的结束时间，【注：只精确到毫秒】
   *
   * @param date
   * @return
   */
  public static Date getWithDrawOfDate(final Date date) {
    Calendar cal = GregorianCalendar.getInstance();
    cal.setTime(date);

    cal.set(Calendar.HOUR_OF_DAY, 13);
    cal.set(Calendar.MINUTE, 00);
    cal.set(Calendar.SECOND, 00);
    cal.set(Calendar.MILLISECOND, 000);

    return new Date(cal.getTime().getTime());
  }


  /**
   * 获得当前日期
   *
   * @return
   */
  public static Date getNow() {
    Calendar cal = Calendar.getInstance();
    Date currDate = cal.getTime();
    return currDate;
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
      String str = format.format(date);
      return str;
    }
    return "";
  }


  /**
   * 根据单位字段比较两个时间
   *
   * @param date      时间1
   * @param otherDate 时间2
   * @param withUnit  单位字段，从Calendar field取值
   * @return 等于返回0值, 大于返回大于0的值 小于返回小于0的值
   */
  public static int compareTime(Date date, Date otherDate, int withUnit) {
    Calendar dateCal = Calendar.getInstance();
    dateCal.setTime(date);
    Calendar otherDateCal = Calendar.getInstance();
    otherDateCal.setTime(otherDate);

    dateCal.clear(Calendar.YEAR);
    dateCal.clear(Calendar.MONTH);
    dateCal.set(Calendar.DATE, 1);
    otherDateCal.clear(Calendar.YEAR);
    otherDateCal.clear(Calendar.MONTH);
    otherDateCal.set(Calendar.DATE, 1);
    switch (withUnit) {
      case Calendar.HOUR:
        dateCal.clear(Calendar.MINUTE);
        otherDateCal.clear(Calendar.MINUTE);
      case Calendar.MINUTE:
        dateCal.clear(Calendar.SECOND);
        otherDateCal.clear(Calendar.SECOND);
      case Calendar.SECOND:
        dateCal.clear(Calendar.MILLISECOND);
        otherDateCal.clear(Calendar.MILLISECOND);
      case Calendar.MILLISECOND:
        break;
      default:
        throw new IllegalArgumentException("withUnit 单位字段 " + withUnit + " 不合法！！");
    }
    return dateCal.compareTo(otherDateCal);
  }

  /**
   * 获得当前的日期毫秒
   *
   * @return
   */
  public static long nowTimeMillis() {
    return System.currentTimeMillis();
  }


  /**
   * 计算 second 秒后的时间
   *
   * @param date
   * @param second
   * @return
   */
  public static Date addSecond(Date date, int second) {
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(date);
    ;
    calendar.add(Calendar.SECOND, second);
    return calendar.getTime();
  }

  /**
   * 计算 minute 分钟后的时间
   *
   * @param date
   * @param minute
   * @return
   */
  public static Date addMinute(Date date, int minute) {
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(date);
    calendar.add(Calendar.MINUTE, minute);
    return calendar.getTime();
  }

  /**
   * 计算 hour 小时后的时间
   *
   * @param date
   * @return
   */
  public static Date addHour(Date date, int hour) {
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(date);
    calendar.add(Calendar.HOUR, hour);
    return calendar.getTime();
  }

  /**
   * 得到day的起始时间点。
   *
   * @param date
   * @return
   */
  public static Date getDayStart(Date date) {
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(date);
    calendar.set(Calendar.HOUR_OF_DAY, 0);
    calendar.set(Calendar.MINUTE, 0);
    calendar.set(Calendar.SECOND, 0);
    calendar.set(Calendar.MILLISECOND, 0);
    return calendar.getTime();
  }

  /**
   * 得到day的终止时间点.
   *
   * @param date
   * @return
   */
  public static Date getDayEnd(Date date) {
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(date);
    calendar.set(Calendar.HOUR_OF_DAY, 0);
    calendar.set(Calendar.MINUTE, 0);
    calendar.set(Calendar.SECOND, 0);
    calendar.set(Calendar.MILLISECOND, 0);
    calendar.add(Calendar.DAY_OF_MONTH, 1);
    calendar.add(Calendar.MILLISECOND, -1);
    return calendar.getTime();
  }

  /**
   * 计算 day 天后的时间
   *
   * @param date
   * @param day
   * @return
   */
  public static Date addDay(Date date, int day) {
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(date);
    calendar.add(Calendar.DAY_OF_MONTH, day);
    return calendar.getTime();
  }

  /**
   * 得到month的终止时间点.
   *
   * @param date
   * @return
   */
  public static Date getMonthEnd(Date date) {
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(date);
    calendar.set(Calendar.DAY_OF_MONTH, 1);
    calendar.set(Calendar.HOUR_OF_DAY, 0);
    calendar.set(Calendar.MINUTE, 0);
    calendar.set(Calendar.SECOND, 0);
    calendar.set(Calendar.MILLISECOND, 0);
    calendar.add(Calendar.MONTH, 1);
    calendar.add(Calendar.MILLISECOND, -1);
    return calendar.getTime();
  }

  public static Date addYear(Date date, int year) {
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(date);
    calendar.add(Calendar.DAY_OF_YEAR, 365 * year);
    return calendar.getTime();
  }


  /**
   * 返回java.sql.Timestamp型的SYSDATE
   *
   * @return java.sql.Timestamp型的SYSDATE
   * @history
   * @since 1.0
   */
  public static java.sql.Timestamp getSysDateTimestamp() {
    return new java.sql.Timestamp(System.currentTimeMillis());
  }


  /**
   * 日期转换为字符串 MM月dd日 hh:mm
   *
   * @param date
   * @return
   */
  public static String dateStr(Date date) {
    return dateStr(date, DATEFORMAT_STR_023);
  }


  /**
   * 日期转换为字符串 yyyy-MM-dd
   *
   * @param date
   * @return
   */
  public static String dateStr2(Date date) {
    return dateStr(date, DATEFORMAT_STR_002);
  }

  /**
   * yyyy年MM月dd日HH时mm分ss秒
   *
   * @param date
   * @return
   */
  public static String dateStr5(Date date) {
    return dateStr(date, DATEFORMAT_STR_021);
  }

  public static String dateStr9(Date date) {
    return dateStr(date, DATEFORMAT_STR_006);
  }

  /**
   * yyyyMMddHHmmss
   *
   * @param date
   * @return
   */
  public static String dateStr3(Date date) {
    return dateStr(date, DATEFORMAT_STR_011);
  }

  /**
   * yyyy-MM-dd HH:mm:ss
   *
   * @param date
   * @return
   */
  public static String dateStr4(Date date) {
    return dateStr(date, DATEFORMAT_STR_001);

  }

  /**
   * yyyy年MM月dd日
   *
   * @param date
   * @return
   */
  public static String dateStr6(Date date) {
    return dateStr(date, DATEFORMAT_STR_022);
  }

  /**
   * yyyyMMdd
   *
   * @param date
   * @return
   */
  public static String dateStr7(Date date) {
    return dateStr(date, DATEFORMAT_STR_012);
  }

  /**
   * MM-dd
   *
   * @param date
   * @return
   */
  public static String dateStr8(Date date) {
    return dateStr(date, DATEFORMAT_STR_003);
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

  public static String dateStr(String times) {
    return dateStr(getDate(times));
  }

  public static String dateStr2(String times) {
    return dateStr2(getDate(times));
  }

  public static String dateStr3(String times) {
    return dateStr3(getDate(times));
  }

  public static String dateStr4(String times) {
    return dateStr4(getDate(times));
  }

  public static String dateStr5(String times) {
    return dateStr5(getDate(times));
  }

  /**
   * 将Date转换为时间戳
   *
   * @param date
   * @return
   */
  public static long getTimestamp(Date date) {
    return date.getTime() / 1000;
  }


  /**
   * 默认的valueOf 方法，格式化 yyyy-mm-dd HH:mm:ss
   *
   * @param str
   * @return
   */
  public static Date valueOf(String str) {
    return valueOf(str, DATEFORMAT_STR_001);
  }

  public static String valueOf9(String str) {
    Date date = valueOf(str, DATEFORMAT_STR_006);
    return dateStr9(date);
  }

  public static long getTimestamp(String str) {
    return valueOf(str, DATEFORMAT_STR_001).getTime();
  }

  /**
   * yyyymmdd 格式化 成 yyyy-mm-dd
   *
   * @param str
   * @return
   */
  public static String formatDate(String str) {
    SimpleDateFormat sf1 = new SimpleDateFormat("yyyyMMdd");
    SimpleDateFormat sf2 = new SimpleDateFormat("yyyy-MM-dd");
    String sfstr = "";
    try {
      sfstr = sf2.format(sf1.parse(str));
    } catch (ParseException e) {
      e.printStackTrace();
    }
    return sfstr;
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
    Date strtoDate = formatter.parse(str, pos);
    return strtoDate;
  }


  /**
   * 前/后?分钟
   *
   * @param d
   * @param minute
   * @return
   */
  public static Date rollMinute(Date d, int minute) {
    return new Date(d.getTime() + minute * 60 * 1000L);
  }

  /**
   * 前/后?小时
   *
   * @param d
   * @param hour
   * @return
   */
  public static Date rollHour(Date d, int hour) {
    Calendar cal = Calendar.getInstance();
    cal.setTime(d);
    cal.add(Calendar.HOUR_OF_DAY, hour);
    return cal.getTime();
  }

  /**
   * 前/后?天
   *
   * @param d
   * @param day
   * @return
   */
  public static Date rollDay(Date d, int day) {
    Calendar cal = Calendar.getInstance();
    cal.setTime(d);
    cal.add(Calendar.DAY_OF_MONTH, day);
    return cal.getTime();
  }

  /**
   * 前/后?月
   *
   * @param d
   * @param mon
   * @return
   */
  public static Date rollMon(Date d, int mon) {
    Calendar cal = Calendar.getInstance();
    cal.setTime(d);
    cal.add(Calendar.MONTH, mon);
    return cal.getTime();
  }

  /**
   * 前/后?年
   *
   * @param d
   * @param year
   * @return
   */
  public static Date rollYear(Date d, int year) {
    Calendar cal = Calendar.getInstance();
    cal.setTime(d);
    cal.add(Calendar.YEAR, year);
    return cal.getTime();
  }

  public static Date rollDate(Date d, int year, int mon, int day) {
    Calendar cal = Calendar.getInstance();
    cal.setTime(d);
    cal.add(Calendar.YEAR, year);
    cal.add(Calendar.MONTH, mon);
    cal.add(Calendar.DAY_OF_MONTH, day);
    return cal.getTime();
  }

  /**
   * 获取当前时间-时间戳字符串
   *
   * @return
   */
  public static String getNowTimeStr() {
    String str = Long.toString(System.currentTimeMillis() / 1000);
    return str;
  }

  /**
   * 将Date转换为时间戳
   *
   * @param time
   * @return
   */
  public static String getTimeStr(Date time) {
    long date = time.getTime();
    String str = Long.toString(date / 1000);
    return str;
  }

  public static String getTimeStr(String dateStr, String format) {
    SimpleDateFormat sdf = new SimpleDateFormat(format);
    Date date;
    try {
      date = sdf.parse(dateStr);
    } catch (ParseException e) {
      e.printStackTrace();
      return "";
    }
    String str = DateTimeUtil.getTimeStr(date);
    return str;
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

  public static Date getLastSecIntegralTime(Date d) {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(d.getTime());
    cal.set(Calendar.HOUR_OF_DAY, 23);
    cal.set(Calendar.SECOND, 59);
    cal.set(Calendar.MINUTE, 59);
    cal.set(Calendar.MILLISECOND, 0);
    return cal.getTime();
  }

  public static long getTime(String format) {
    long t = 0;
    if (StringUtil.isBlank(format)) {
      return t;
    }
    SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
    Date date;
    try {
      date = sdf.parse(format);
      t = date.getTime() / 1000;
    } catch (ParseException e) {
      e.printStackTrace();
    }
    return t;
  }

  // 获取本周日的日期
  @SuppressWarnings("unused")
  public static String getCurrentWeekday() {
    int weeks = 0;
    int mondayPlus = DateTimeUtil.getMondayPlus();
    GregorianCalendar currentDate = new GregorianCalendar();
    currentDate.add(GregorianCalendar.DATE, mondayPlus + 6);
    Date monday = currentDate.getTime();

    //DateFormat df = DateFormat.getDateInstance();
    SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd");
    String preMonday = df.format(monday);
    return preMonday;
  }

  // 获得当前日期与本周日相差的天数
  private static int getMondayPlus() {
    Calendar cd = Calendar.getInstance();
    // 获得今天是一周的第几天，星期日是第一天，星期二是第二天......
    int dayOfWeek = cd.get(Calendar.DAY_OF_WEEK) - 1; // 因为按中国礼拜一作为第一天所以这里减1
    if (dayOfWeek == 1) {
      return 0;
    } else {
      return 1 - dayOfWeek;
    }
  }

  // 获得本周一的日期
  @SuppressWarnings("unused")
  public static String getMondayOFWeek() {
    int weeks = 0;
    int mondayPlus = DateTimeUtil.getMondayPlus();
    GregorianCalendar currentDate = new GregorianCalendar();
    currentDate.add(GregorianCalendar.DATE, mondayPlus);
    Date monday = currentDate.getTime();

    //DateFormat df = DateFormat.getDateInstance();
    SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd");
    String preMonday = df.format(monday);
    return preMonday;
  }

  // 获取当前月第一天：
  public static String getFirstDayOfMonth() {
    SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd");
    Calendar c = Calendar.getInstance();
    c.add(Calendar.MONTH, 0);
    c.set(Calendar.DAY_OF_MONTH, 1);// 设置为1号,当前日期既为本月第一天
    String first = format.format(c.getTime());
    return first;
  }

  // 获取当月最后一天
  public static String getLastDayOfMonth() {
    SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd");
    Calendar ca = Calendar.getInstance();
    ca.set(Calendar.DAY_OF_MONTH, ca.getActualMaximum(Calendar.DAY_OF_MONTH));
    String last = format.format(ca.getTime());
    return last;
  }

  /**
   * 日期月份处理
   *
   * @param d     时间
   * @param month 相加的月份，正数则加，负数则减
   * @return
   */
  public static Date timeMonthManage(Date d, int month) {
    Calendar rightNow = Calendar.getInstance();
    rightNow.setTime(d);
    rightNow.add(Calendar.MONTH, month);
    return rightNow.getTime();
  }

  /**
   * 获取指定年月的最后一天
   *
   * @param year_time  指定年
   * @param month_time 指定月
   * @return
   */
  public static Date monthLastDay(int year_time, int month_time) {
    Calendar cal = Calendar.getInstance();
    cal.set(year_time, month_time, 0, 23, 59, 59);
    return cal.getTime();
  }

  /**
   * 获取指定年月的第一天
   *
   * @param year_time  指定年
   * @param month_time 指定月
   * @return
   */
  public static Date monthFirstDay(int year_time, int month_time) {
    Calendar cal = Calendar.getInstance();
    cal.set(year_time, month_time - 1, 1, 0, 0, 0);
    return cal.getTime();
  }

  /**
   * 获取指定时间月的第一天
   *
   * @param d 指定时间，为空代表当前时间
   * @return
   */
  public static Date currMonthFirstDay(Date d) {
    Calendar cal = Calendar.getInstance();
    if (d != null) {
      cal.setTime(d);
    }
    cal.set(Calendar.DAY_OF_MONTH, cal.getActualMinimum(Calendar.DAY_OF_MONTH));
    cal.set(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DATE), 0, 0, 0);
    return cal.getTime();
  }

  /**
   * 获取指定时间月的最后一天
   *
   * @param d 指定时间，为空代表当前时间
   * @return
   */
  public static Date currMonthLastDay(Date d) {
    Calendar cal = Calendar.getInstance();
    if (d != null) {
      cal.setTime(d);
    }
    cal.set(Calendar.DAY_OF_MONTH, cal.getActualMaximum(Calendar.DAY_OF_MONTH));
    cal.set(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DATE), 23, 59, 59);
    return cal.getTime();
  }

  /**
   * 获取指定时间的年
   *
   * @param date 指定时间
   * @return
   */
  public static int getTimeYear(Date date) {
    if (date == null) {
      date = new Date();
    }
    Calendar cal = Calendar.getInstance();
    cal.setTime(date);
    return cal.get(Calendar.YEAR);
  }

  /**
   * 获取指定时间的月
   *
   * @param date 指定时间
   * @return
   */
  public static int getTimeMonth(Date date) {
    if (date == null) {
      date = new Date();
    }
    Calendar cal = Calendar.getInstance();
    cal.setTime(date);
    return cal.get(Calendar.MONTH) + 1;
  }

  /**
   * 获取指定时间的天
   *
   * @param date 指定时间
   * @return
   */
  public static int getTimeDay(Date date) {
    if (date == null) {
      date = new Date();
    }
    Calendar cal = Calendar.getInstance();
    cal.setTime(date);
    return cal.get(Calendar.DATE);
  }

  public static Date getFirstSecIntegralTime(Date d) {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(d.getTime());
    cal.set(Calendar.HOUR_OF_DAY, 0);
    cal.set(Calendar.SECOND, 0);
    cal.set(Calendar.MINUTE, 0);
    cal.set(Calendar.MILLISECOND, 0);
    cal.set(Calendar.DATE, 0);
    return cal.getTime();
  }

  /**
   * 指定天数 d + day天后的结束时间
   *
   * @param d
   * @param day
   * @return
   */
  public static Date rollDayEndTime(Date d, int day) {
    Calendar cal = Calendar.getInstance();
    cal.setTime(d);
    cal.add(Calendar.DAY_OF_MONTH, day);
    cal.set(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DATE), 23, 59, 59);
    return cal.getTime();
  }

  /**
   * 指定天数 d + day天后的开始时间
   *
   * @param d
   * @param day
   * @return
   */
  public static Date rollDayStartTime(Date d, int day) {
    Calendar cal = Calendar.getInstance();
    cal.setTime(d);
    cal.add(Calendar.DAY_OF_MONTH, day);
    cal.set(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DATE), 00, 00, 00);
    return cal.getTime();
  }

  /**
   * 获取指定时间天的结束时间
   *
   * @param d
   * @return
   */
  public static Date getDayEndTime(long d) {
    Date day = null;
    if (d <= 0) {
      day = new Date();
    } else {
      day = new Date(d * 1000);
    }
    Calendar cal = Calendar.getInstance();
    if (day != null) {
      cal.setTimeInMillis(day.getTime());
    }
    cal.set(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DATE), 23, 59, 59);
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
   * 获取指定时间天的开始时间
   *
   * @return
   */
  public static Date getDayStartTime(Date day) {
    Calendar cal = Calendar.getInstance();
    if (day != null) {
      cal.setTimeInMillis(day.getTime());
    }
    cal.set(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DATE), 0, 0, 0);
    return cal.getTime();
  }

  /**
   * 获取指定时间天的开始时间
   *
   * @param d
   * @return
   */
  public static Date getDayStartTime(long d) {
    Date day = null;
    if (d <= 0) {
      day = new Date();
    } else {
      day = new Date(d * 1000);
    }
    Calendar cal = Calendar.getInstance();
    if (day != null) {
      cal.setTimeInMillis(day.getTime());
    }
    cal.set(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DATE), 0, 0, 0);
    return cal.getTime();
  }

  /**
   * 获取19位的格式时间
   *
   * @param dateStr
   * @return
   * @throws ParseException
   */
  public static Date getDateByFullDateStr(String dateStr) {
    if (StringUtil.isBlank(dateStr)) {
      return null;
    }
    try {
      SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
      return sdf.parse(dateStr);
    } catch (Exception e) {
      e.printStackTrace();
      return null;
    }
  }

  /**
   * 获取10位的格式时间
   *
   * @param dateStr
   * @return
   * @throws ParseException
   */
  public static Date getDateByDateStr(String dateStr) {
    if (StringUtil.isBlank(dateStr)) {
      return null;
    }
    try {
      SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
      return sdf.parse(dateStr);
    } catch (Exception e) {
      e.printStackTrace();
      return null;
    }
  }

  /**
   * 获取10位的格式时间
   *
   * @param dateStr
   * @return
   * @throws ParseException
   */
  public static Date getDateByDateMdStr(String dateStr) {
    if (StringUtil.isBlank(dateStr)) {
      return null;
    }
    try {
      SimpleDateFormat sdf = new SimpleDateFormat("yyyy-m-d");
      return sdf.parse(dateStr);
    } catch (Exception e) {
      e.printStackTrace();
      return null;
    }
  }

  /**
   * 计算两个日期之间相差的天数
   *
   * @param date1
   * @param date2
   * @return
   */
  public static int daysBetween(Date date1, Date date2) {
    DateFormat sdf = new SimpleDateFormat(DATEFORMAT_STR_012);
    Calendar cal = Calendar.getInstance();
    try {
      Date d1 = sdf.parse(DateTimeUtil.dateStr7(date1));
      Date d2 = sdf.parse(DateTimeUtil.dateStr7(date2));
      cal.setTime(d1);
      long time1 = cal.getTimeInMillis();
      cal.setTime(d2);
      long time2 = cal.getTimeInMillis();
      return Integer.parseInt(String.valueOf((time2 - time1) / 86400000L));
    } catch (ParseException e) {
      e.printStackTrace();
    }
    return 0;
  }

  /**
   * 计算两个日期之间相差的小时数
   *
   * @param date1
   * @param date2
   * @return
   */
  public static int hoursBetween(Date date1, Date date2) {

    DateFormat sdf = new SimpleDateFormat(DATEFORMAT_STR_012);
    Calendar cal = Calendar.getInstance();
    try {
      Date d1 = sdf.parse(DateTimeUtil.dateStr7(date1));
      Date d2 = sdf.parse(DateTimeUtil.dateStr7(date2));
      cal.setTime(d1);
      long time1 = cal.getTimeInMillis();
      cal.setTime(d2);
      long time2 = cal.getTimeInMillis();
      return Integer.parseInt(String.valueOf((time2 - time1) / 3600000L));
    } catch (ParseException e) {
      e.printStackTrace();
    }
    return 0;
  }

  /**
   * 计算两个日期之间相差的分钟数
   *
   * @param date1
   * @param date2
   * @return
   */
  public static int minuteBetween(Date date1, Date date2) {

    DateFormat sdf = new SimpleDateFormat(DATEFORMAT_STR_001);
    Calendar cal = Calendar.getInstance();
    try {
      Date d1 = sdf.parse(DateTimeUtil.dateStr4(date1));
      Date d2 = sdf.parse(DateTimeUtil.dateStr4(date2));
      cal.setTime(d1);
      long time1 = cal.getTimeInMillis();
      cal.setTime(d2);
      long time2 = cal.getTimeInMillis();
      return Integer.parseInt(String.valueOf((time2 - time1) / (1000 * 60)));
    } catch (ParseException e) {
      e.printStackTrace();
    }
    return 0;
  }

  /**
   * 字符串转时间
   *
   * @param str
   * @param dateFormatStr
   * @return
   */
  public static Date StrToDate(String str, String dateFormatStr) {
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
   * 比较两个时间大小
   *
   * @param date1
   * @param date2 date1>date2= ture
   * @return
   */
  public static boolean compare(Date date1, Date date2) {
    DateFormat sdf = new SimpleDateFormat(DATEFORMAT_STR_001);
    Calendar cal = Calendar.getInstance();
    boolean b = false;
    try {
      Date d1 = sdf.parse(DateTimeUtil.dateStr4(date1));
      Date d2 = sdf.parse(DateTimeUtil.dateStr4(date2));
      cal.setTime(d1);
      long time1 = cal.getTimeInMillis();
      cal.setTime(d2);
      long time2 = cal.getTimeInMillis();
      if (time1 > time2) {
        b = true;
      }
    } catch (ParseException e) {
      e.printStackTrace();
    }
    return b;
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
    Date time = cal.getTime();
    return time;

  }


  /**
   * 获取昨天 开始时间
   *
   * @return
   */
  public static Date getSimpleYesterday() {
    Calendar cal = Calendar.getInstance();
    cal.add(Calendar.DATE, -1);
    cal.set(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DATE));
    Date time = cal.getTime();
    return time;

  }


  /**
   * 获取昨天 开始时间
   *
   * @return
   */
  public static String getSimpleYesterdayStr() {
    Calendar cal = Calendar.getInstance();
    cal.add(Calendar.DATE, -1);
    cal.set(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DATE));
    Date time = cal.getTime();
    SimpleDateFormat sdf = new SimpleDateFormat(DATEFORMAT_STR_002);
    return sdf.format(time);
  }


  public static final int FIRST_DAY = Calendar.MONDAY;

  public static String getWeekStartdays() {
    Calendar calendar = Calendar.getInstance();
    setToFirstDay(calendar);

    Date date = DateTimeUtil.getDayStartTime(calendar.getTime());
    SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
    return dateFormat.format(date);
  }

  public static final int REAL_FIRST_DAY = Calendar.SUNDAY;

  public static final String DATE_FORMAT_DATETIME = "yyyy-MM-dd HH:mm:ss";
  public static final String DATE_FORMAT_DATEONLY = "yyyy-MM-dd";

  public static String getRealWeekStartdays() {
    Calendar calendar = Calendar.getInstance();
    setToRealFirstDay(calendar);

    Date date = DateTimeUtil.getDayStartTime(calendar.getTime());
    SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
    return dateFormat.format(date);
  }

  private static void setToRealFirstDay(Calendar calendar) {
    while (calendar.get(Calendar.DAY_OF_WEEK) != REAL_FIRST_DAY) {
      calendar.add(Calendar.DATE, -1);
    }
  }

  public static String getRealWeekEnddays() {
    Calendar calendar = Calendar.getInstance();
    setToRealFirstDay(calendar);
    calendar.add(Calendar.DATE, 6);
    Date date = DateTimeUtil.getDayEndTime(calendar.getTime());
    SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    return dateFormat.format(date);
  }

  public static String getWeekEnddays() {
    Calendar calendar = Calendar.getInstance();
    setToFirstDay(calendar);
    calendar.add(Calendar.DATE, 6);
    Date date = DateTimeUtil.getDayEndTime(calendar.getTime());
    SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    return dateFormat.format(date);
  }

  private static void setToFirstDay(Calendar calendar) {
    while (calendar.get(Calendar.DAY_OF_WEEK) != FIRST_DAY) {
      calendar.add(Calendar.DATE, -1);
    }
  }


  static Date parseDate(String str, String[] strings) {
    return null;
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
    Date time = cal.getTime();
    return time;
  }


  public static int getCurrentHour() {
    Calendar cal = Calendar.getInstance();
    int a = cal.get(Calendar.HOUR_OF_DAY);
    return a;
  }

  /**
   * 获取当前日期字符串
   *
   * @return
   */
  public static String getCurrentDateStr() {
    SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");//等价于now.toLocaleString()
    return sdf.format(DateTimeUtil.getNow());
  }


  /**
   * 获取当前简单的年月日
   *
   * @return
   */
  public static String getCurrentSimpleDateStr() {
    SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMdd");//等价于now.toLocaleString()
    return sdf.format(DateTimeUtil.getNow());
  }

  public static boolean isSameDay(Date date1, Date date2) {
    Calendar cal1 = Calendar.getInstance();
    cal1.setTime(date1);
    Calendar cal2 = Calendar.getInstance();
    cal2.setTime(date2);
    boolean isSameYear = cal1.get(Calendar.YEAR) == cal2.get(Calendar.YEAR);
    boolean isSameMonth = isSameYear
      && cal1.get(Calendar.MONTH) == cal2.get(Calendar.MONTH);
    boolean isSameDate = isSameMonth
      && cal1.get(Calendar.DAY_OF_MONTH) == cal2
      .get(Calendar.DAY_OF_MONTH);
    return isSameDate;
  }

  /**
   * 获取格式化时间
   *
   * @param enterTime
   * @return
   */
  public static String getFormateDateStr(Date enterTime) {
    SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyyMMdd HH:mm:ss");
    return simpleDateFormat.format(enterTime);
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
      sRet = formatter.format(dt).toString();
    } catch (Exception e) {
      sRet = null;
    }

    return sRet;
  }

  /**
   * @param s
   * @param e
   * @return boolean
   * @Title: compareDate
   * @Description: (日期比较 ， 如果s > = e 返回true 否则返回false)
   */
  public static boolean compareDate(Date s, Date e) {
    return s.getTime() >= e.getTime();
  }

  public static String formatDateTime(Date date) {
    return formatDate(date, DATE_TIME_SHORT);
  }

  public Integer getTimeDifferenceDays(String time) {
    SimpleDateFormat date = new SimpleDateFormat(STANDARD_FORMAT);
    String endTime = time;
    String nowDate = date.format(new Date());
    Date now = null;
    Date end = null;
    try {
      now = date.parse(nowDate);
      end = date.parse(endTime);
    } catch (ParseException e) {
      e.printStackTrace();
    }
    return Days.daysBetween(new DateTime(end), new DateTime(now)).getDays();
  }

  public static Integer getTimeDifferenceSeconds(String time) {
    SimpleDateFormat date = new SimpleDateFormat(STANDARD_FORMAT);
    String endTime = time;
    String nowDate = date.format(new Date());
    Date now = null;
    Date end = null;
    try {
      now = date.parse(nowDate);
      end = date.parse(endTime);
    } catch (ParseException e) {
      e.printStackTrace();
    }
    return Seconds.secondsBetween(new DateTime(end), new DateTime(now)).getSeconds() % 60;
  }

  public static String timeLongToString(Long startTime) {
    SimpleDateFormat sdf = new SimpleDateFormat(STANDARD_FORMAT);
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(startTime);
    String time = sdf.format(cal.getTime());
    return time;
  }

  /**
   * @return long
   * @Author zhaoming
   * @Description 获取两个时间差（秒）
   * @Date 16:43 2020/1/21
   * @Param []
   **/
  public static long getSecond(Instant fromDate) {
    Instant toDate = Instant.now();
    long seconds = ChronoUnit.SECONDS.between(fromDate, toDate);
    return seconds;
  }

  /**
   * 日期转换成字符串
   *
   * @param date
   * @return str
   */
  public static String DateToStr(Date date) {
    SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
    String str = format.format(date);
    return str;
  }

  public static String DateToStr2(Date date) {
    SimpleDateFormat format = new SimpleDateFormat(STANDARD_FORMAT_T);
    String str = format.format(date);
    return str;
  }

  public static String DateToStrYYYYMMDDHHMMSS(Date date) {
    SimpleDateFormat format = new SimpleDateFormat(STANDARD_FORMAT_yyyy_MM_dd_HHmmssSSSSSS);
    String str = format.format(date);
    return str;
  }

  public static Long DateToStrYYYYMMDDHHMMSS2(Date date) {
    SimpleDateFormat format = new SimpleDateFormat(STANDARD_FORMAT_yyyyMMddHHmmssSSS);
    String str = format.format(date);
    Long aLong = Long.valueOf(str);
    return aLong;
  }

  public static String DateToStrYYYYMMDD(Date date) {
    SimpleDateFormat format = new SimpleDateFormat(STANDARD_FORMAT_yyyyMMdd);
    String str = format.format(date);
    return str;
  }

  /**
   * 字符串转换成日期
   *
   * @param str
   * @return date
   */
  public static Date strToDate(String str) {
    SimpleDateFormat format = new SimpleDateFormat(STANDARD_FORMAT);
    Date date = null;
    try {
      date = format.parse(str);
    } catch (Exception e) {
      e.printStackTrace();
    }
    return date;
  }

  public static Date strToDate(String dateTimeStr, String formatStr) {
    DateTimeFormatter dateTimeFormatter = DateTimeFormat.forPattern(formatStr);
    DateTime dateTime = dateTimeFormatter.parseDateTime(dateTimeStr);
    return dateTime.toDate();
  }

  public static String strToDateToStr(String dateTimeStr, String formatStr) {
    DateTimeFormatter dateTimeFormatter = DateTimeFormat.forPattern(formatStr);
    DateTime dateTime = dateTimeFormatter.parseDateTime(dateTimeStr);
    return DateToStr(dateTime.toDate());
  }

  public static String strToDateToStr2(String dateTimeStr, String formatStr) {
    DateTimeFormatter dateTimeFormatter = DateTimeFormat.forPattern(formatStr);
    DateTime dateTime = dateTimeFormatter.parseDateTime(dateTimeStr);
    return DateToStr2(dateTime.toDate());
  }

  public static String dateToStr(Date date, String formatStr) {
    if (date == null) {
      return StringUtils.EMPTY;
    }
    DateTime dateTime = new DateTime(date);
    return dateTime.toString(formatStr);
  }

  public static String dateToStr(Date date) {
    if (date == null) {
      return StringUtils.EMPTY;
    }
    DateTime dateTime = new DateTime(date);
    return dateTime.toString(STANDARD_FORMAT);
  }

  public static String dateToStrformathhmmss(Date date) {
    if (date == null) {
      return StringUtils.EMPTY;
    }
    DateTime dateTime = new DateTime(date);
    return dateTime.toString(STANDARD_FORMAT_HHMMSS);
  }

  public static String dateToStrformathhmm(Date date) {
    if (date == null) {
      return StringUtils.EMPTY;
    }
    DateTime dateTime = new DateTime(date);
    return dateTime.toString(STANDARD_FORMAT_HHMM);
  }

  public static String dateToStrformat(Date date) {
    if (date == null) {
      return StringUtils.EMPTY;
    }
    DateTime dateTime = new DateTime(date);
    return dateTime.toString(STANDARD_FORMAT);
  }

}
