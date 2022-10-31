package com.mingshi.skyflying.anomaly_detection.config;

/**
 * <B>类名称：InitDemoMode</B>
 * <B>概要说明：是否开启演示模式</B>
 *
 * @Author zm
 * @Date 2022/10/31 13:45
 **/
public class InitDemoMode {
    private static volatile Boolean isDemoMode = Boolean.FALSE;

    public static Boolean getIsDemoMode() {
        return isDemoMode;
    }

    public static void setIsDemoMode(Boolean flag){
        isDemoMode = flag;
    }
}
