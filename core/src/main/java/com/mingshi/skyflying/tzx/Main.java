package com.mingshi.skyflying.tzx;

/**
 * @Author: 唐郑翔
 * @Description:
 * @Date: create in 2022/8/17
 */
public class Main {

}

class Solution {
    public int maxProfit(int[] prices, int fee) {
        int n = prices.length;
        if(n == 0) {
            return 0;
        }
        int[][] dp = new int[n][2];
        dp[0][0] = -prices[0];
        for(int i = 1; i < n; i++) {
            dp[i][0] = Math.max(dp[i-1][0], dp[i-1][1] + prices[i] - fee);
            dp[i][1] = Math.max(dp[i-1][1], dp[i-1][0] - prices[i]);
        }
        return dp[n-1][0];
    }
}
