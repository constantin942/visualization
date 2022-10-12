package com.mingshi.skyflying.common.utils;

import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.enums.ConstantsCode;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

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

  private DateTimeUtil(){}
  public static final String STANDARD_FORMAT = "yyyy-MM-dd HH:mm:ss";
  public static final String STANDARD_FORMAT_T = "yyyy-MM-dd'T'HH:mm:ss";
  public static final String STANDARD_FORMAT_YYYYMMDD = "yyyy-MM-dd";
  public static final String STANDARD_FORMAT_YYYYMMDDHHMMSSSSS = "yyyyMMddHHmmssSSS";
  public static final String STANDARD_FORMAT_YYYY_MM_DD_HHMMSSSSSSSS = "yyyy-MM-dd HH:mm:ss.SSSSSS";
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
   * <B>方法名称：removeDays</B>
   * <B>概要说明：日期减天数</B>
   * @Author zm
   * @Date 2022年08月01日 14:08:25
   * @Param [oldDate, n]
   * @return java.util.Date
   **/
  public static Date removeDays(Date oldDate, int n) {

    Date newDate = null;
    Calendar calOld = Calendar.getInstance();
    calOld.setTime(oldDate);
    int day = calOld.get(Calendar.DAY_OF_YEAR);
    Calendar calNew = Calendar.getInstance();
    calNew.setTime(oldDate);
    calNew.set(Calendar.DAY_OF_YEAR, day - n);
    newDate = calNew.getTime();
    return newDate;

  }

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
    return LocalDateTime.parse(date, java.time.format.DateTimeFormatter.ofPattern(STANDARD_FORMAT)
      .withZone(ZoneId.systemDefault()).withLocale(Locale.CHINA)).toInstant(ZoneOffset.UTC);
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
    return LocalDateTime.parse(date, java.time.format.DateTimeFormatter.ofPattern(dateFormat)
      .withZone(ZoneId.systemDefault()).withLocale(Locale.CHINA)).toInstant(ZoneOffset.UTC);
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

  /**
   * 格式化日期
   *
   * @param date
   * @return
   */
  public static Date formatDateToYyyyMmDd(Date date) {
    if (date == null) {
      return null;
    }
    SimpleDateFormat sdf = new SimpleDateFormat(DATEFORMAT_STR_002);
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
    return formatDate(date, DATEFORMAT_STR_002);
  }


  public static String formatDate(final Date date, String format) {
    return new SimpleDateFormat(format).format(date);
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
    return Math.abs((timeEnd - timeFrom) / 1000);
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
    return Math.abs((timeEnd - timeFrom));
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
    List<String> dateList = new LinkedList<>();
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
        dateList.add(startTimeStr);
        startTimeCalendar.add(Calendar.DATE, 1);
      }

      if (startTimeCalendar.before(endTimeCalendar)) {
        String endTimeStr = sdf.format(startTimeCalendar.getTime());
        dateList.add(endTimeStr);
      }
    } catch (Exception e) {
      log.error("# DateTimeUtil.getDateList() # 转换时间时，出现了异常。", e);
    }
    return dateList;
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
    //上午开始时间
    amBegin.setTime(amBeginTime);
    //设置结束时间
    Calendar amEnd = Calendar.getInstance();
    //上午结束时间
    amEnd.setTime(amEndTime);
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
    if ((a >= Const.NUMBER_ZERO && a <= Const.NUMBER_SIX) || (a > Const.NUMBER_EIGHTEEN && a <= Const.NUMBER_TWENTY_FOUR)) {
      return ConstantsCode.USER_PORTRAIT_NIGHT.getCode();
    }
    if (a > Const.NUMBER_SIX && a <= Const.NUMBER_TWELVE) {
      return ConstantsCode.USER_PORTRAIT_FORENOON.getCode();
    }
    if (a > Const.NUMBER_TWELVE && a <= Const.NUMBER_EIGHTEEN) {
      return ConstantsCode.USER_PORTRAIT_AFTERNOON.getCode();
    }
    return null;
  }

  /**
   * 返回一个Date默认最大值
   *
   * @return
   */
  public static Date getFinalDate() {
    return parseDate(FINAL_END_DATE_STR, DATEFORMAT_STR_002);
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
   * 默认的valueOf 方法，格式化 yyyy-mm-dd HH:mm:ss
   *
   * @param str
   * @return
   */
  public static Date valueOf(String str) {
    return valueOf(str, DATEFORMAT_STR_001);
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

  public static long getTime(String format) {
    long t = 0;
    if (StringUtil.isBlank(format)) {
      return t;
    }
    SimpleDateFormat sdf = new SimpleDateFormat(STANDARD_FORMAT);
    Date date;
    try {
      date = sdf.parse(format);
      t = date.getTime() / 1000;
    } catch (ParseException e) {
      e.printStackTrace();
    }
    return t;
  }

  /**
   * 获取本周日的日期
   * @return
   */
  @SuppressWarnings("unused")
  public static String getCurrentWeekday() {
    int weeks = 0;
    int mondayPlus = DateTimeUtil.getMondayPlus();
    GregorianCalendar currentDate = new GregorianCalendar();
    currentDate.add(GregorianCalendar.DATE, mondayPlus + 6);
    Date monday = currentDate.getTime();

    SimpleDateFormat df = new SimpleDateFormat(DATEFORMAT_STR_002);
    return df.format(monday);
  }

  /**
   * 获得当前日期与本周日相差的天数
   * @return
   */
  private static int getMondayPlus() {
    Calendar cd = Calendar.getInstance();
    // 获得今天是一周的第几天，星期日是第一天，星期二是第二天......
    // 因为按中国礼拜一作为第一天所以这里减1
    int dayOfWeek = cd.get(Calendar.DAY_OF_WEEK) - 1;
    if (dayOfWeek == 1) {
      return 0;
    } else {
      return 1 - dayOfWeek;
    }
  }

  /**
   * 获得本周一的日期
   * @return
   */
  @SuppressWarnings("unused")
  public static String getMondayOfWeek() {
    int weeks = 0;
    int mondayPlus = DateTimeUtil.getMondayPlus();
    GregorianCalendar currentDate = new GregorianCalendar();
    currentDate.add(GregorianCalendar.DATE, mondayPlus);
    Date monday = currentDate.getTime();

    SimpleDateFormat df = new SimpleDateFormat(DATEFORMAT_STR_002);
    return df.format(monday);
  }

  /**
   * 获取当前月第一天：
   * @return
   */
  public static String getFirstDayOfMonth() {
    SimpleDateFormat format = new SimpleDateFormat(DATEFORMAT_STR_002);
    Calendar c = Calendar.getInstance();
    c.add(Calendar.MONTH, 0);
    // 设置为1号,当前日期既为本月第一天
    c.set(Calendar.DAY_OF_MONTH, 1);
    return format.format(c.getTime());
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

  public static final int FIRST_DAY = Calendar.MONDAY;

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
    Date date = DateTimeUtil.getDayEndTime(calendar.getTime());
    SimpleDateFormat dateFormat = new SimpleDateFormat(STANDARD_FORMAT);

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
    SimpleDateFormat sdf = new SimpleDateFormat(DATEFORMAT_STR_002);
    return sdf.format(DateTimeUtil.getNow());
  }


  /**
   * 获取当前简单的年月日
   *
   * @return
   */
  public static String getCurrentSimpleDateStr() {
    SimpleDateFormat sdf = new SimpleDateFormat(DATEFORMAT_STR_002);
    return sdf.format(DateTimeUtil.getNow());
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
   * @return long
   * @Author zhaoming
   * @Description 获取两个时间差（秒）
   * @Date 16:43 2020/1/21
   * @Param []
   **/
  public static long getSecond(Instant fromDate) {
    Instant toDate = Instant.now();
    return ChronoUnit.SECONDS.between(fromDate, toDate);
  }

  /**
   * 日期转换成字符串
   *
   * @param date
   * @return str
   */
  public static String date2Str(Date date) {
    SimpleDateFormat format = new SimpleDateFormat(STANDARD_FORMAT);
    return format.format(date);
  }

  public static String dateToStr2(Date date) {
    SimpleDateFormat format = new SimpleDateFormat(STANDARD_FORMAT_T);
    return format.format(date);
  }

  public static String dateToStrYyyyMmDdHhMmSs(Date date) {
    SimpleDateFormat format = new SimpleDateFormat(STANDARD_FORMAT_YYYY_MM_DD_HHMMSSSSSSSS);
    return format.format(date);
  }

  public static String dateToStrYyyyMmDd(Date date) {
    SimpleDateFormat format = new SimpleDateFormat(STANDARD_FORMAT_YYYYMMDD);
    return format.format(date);
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
    return date2Str(dateTime.toDate());
  }

  public static String strToDateToStr2(String dateTimeStr, String formatStr) {
    DateTimeFormatter dateTimeFormatter = DateTimeFormat.forPattern(formatStr);
    DateTime dateTime = dateTimeFormatter.parseDateTime(dateTimeStr);
    return dateToStr2(dateTime.toDate());
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
