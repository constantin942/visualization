package com.mingshi.skyflying.common.constant;

/**
 * @Author: 唐郑翔
 * @Description:
 * @Date: create in 2022/10/12
 */
public class AnomalyConst {
    private AnomalyConst(){}

    public static final String RULE_PREFIX = "anomaly_detection:enableRule:";

    public static final String MORNING = "morning";

    public static final String AFTERNOON = "afternoon";

    public static final String NIGHT = "night";

    public static final String DEMO_MODE = "demo_mode";

    public static final Integer TABLE_ID = 2;

    public static final Integer TIME_ID = 1;

    public static final String TIME_SUF = "time";

    public static final String TABLE_SUF = "table";

    public static final String TABLE_NAME = "table_name";

    public static final String USER_NAME = "username";

    public static final String COUNTS = "counts";

    public static final String NO_USER = "未知用户";

    public static final String HAVE_USER = "用户";

    public static final Integer SECONDS = 60;

    public static final Integer LOCAL_REDIS_CACHE_SIZE = 2000;

    public static final Integer USER_PORTRAIT_TABLE_LOCAL_CACHE_SIZE = 100 * 10000;

    public static final Integer USER_PORTRAIT_TIME_LOCAL_CACHE_SIZE = 10000;

    public static final Integer USER_PORTRAIT_TIME_PARTITION_LOCAL_CACHE_SIZE = 10000;

    public static final Integer USER_FIRST_VISIT_LOCAL_CACHE_SIZE = 3000;

    public static final Integer LOCAL_REDIS_CACHE_EXPIRE = 5;

    public static final Integer USER_PORTRAIT_LOCAL_CACHE_EXPIRE = 12;

    public static final Integer ALARM_INHIBIT_LOCAL_CACHE_SIZE = 1000;

    public static final String REDIS_TIME_PORTRAIT_PREFIX = "anomaly_detection:portraitByTime:";

    public static final String REDIS_TIME_PARTITION_PORTRAIT_PREFIX = "anomaly_detection:portraitByTimePartition:";

    public static final String REDIS_TABLE_PORTRAIT_PREFIX = "anomaly_detection:portraitByTable:";


    public static final String INTERVAL1 = "interval1";

    public static final String INTERVAL2 = "interval2";

    public static final String INTERVAL3 = "interval3";

    public static final String INTERVAL4 = "interval4";

    public static final String INTERVAL5 = "interval5";

    public static final String INTERVAL6 = "interval6";

    public static final String INTERVAL7 = "interval7";

    public static final String INTERVAL8 = "interval8";

    public static final String INTERVAL9 = "interval9";

    public static final String INTERVAL10 = "interval10";

    public static final String INTERVAL11 = "interval11";

    public static final String INTERVAL12 = "interval12";

    public static final String INTERVAL13 = "interval13";

    public static final String INTERVAL14 = "interval14";

    public static final String INTERVAL15 = "interval15";

    public static final String INTERVAL16 = "interval16";

    public static final String INTERVAL17 = "interval17";

    public static final String INTERVAL18 = "interval18";

    public static final String INTERVAL19 = "interval19";

    public static final String INTERVAL20 = "interval20";

    public static final String INTERVAL21 = "interval21";

    public static final String INTERVAL22 = "interval22";

    public static final String INTERVAL23 = "interval23";

    public static final String INTERVAL24 = "interval24";

    public static final String[] INTERVALS = {INTERVAL1, INTERVAL2
            , INTERVAL3, INTERVAL4, INTERVAL5, INTERVAL6, INTERVAL7
            , INTERVAL8, INTERVAL9, INTERVAL10, INTERVAL11, INTERVAL12
            , INTERVAL13, INTERVAL14, INTERVAL15, INTERVAL16, INTERVAL17
            , INTERVAL18, INTERVAL19, INTERVAL20, INTERVAL21, INTERVAL22
            , INTERVAL23, INTERVAL24};


}
