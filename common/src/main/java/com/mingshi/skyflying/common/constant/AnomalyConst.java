package com.mingshi.skyflying.common.constant;

/**
 * @Author: 唐郑翔
 * @Description:
 * @Date: create in 2022/10/12
 */
public class AnomalyConst {
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

    public static final String USER_NAME = "user_name";

    public static final String COUNTS = "counts";

    public static final String NO_USER = "未知用户";

    public static final String HAVE_USER = "用户";

    public static final Integer SECONDS = 60;

    public static final Integer LOCAL_REDIS_CACHE_SIZE = 2000;

    public static final Integer USER_PORTRAIT_TABLE_LOCAL_CACHE_SIZE = 100 * 10000;

    public static final Integer USER_PORTRAIT_TIME_LOCAL_CACHE_SIZE = 10000;

    public static final Integer USER_FIRST_VISIT_LOCAL_CACHE_SIZE = 3000;

    public static final Integer LOCAL_REDIS_CACHE_EXPIRE = 5;

    public static final Integer USER_PORTRAIT_LOCAL_CACHE_EXPIRE = 12;


    public static final String REDIS_TIME_PORTRAIT_PREFIX = "anomaly_detection:portraitByTime:";

    public static final String REDIS_TABLE_PORTRAIT_PREFIX = "anomaly_detection:portraitByTable:";

}
