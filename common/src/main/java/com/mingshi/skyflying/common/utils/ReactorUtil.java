package com.mingshi.skyflying.common.utils;

import lombok.extern.slf4j.Slf4j;

/**
 * @ClassName MboleUtil
 * @Author apple
 * Date 2021/1/5 下午3:14
 * @Version 1.0
 **/
@Slf4j
public class ReactorUtil {
  private static volatile Boolean TWO_POWER_FLAG_BY_PROCESSOR = null;
  private static volatile Boolean TWO_POWER_FLAG_BY_IOTHREADER = null;

  private ReactorUtil(){}

  /**
   * <B>方法名称：indexFor</B>
   * <B>概要说明：采用按位与代替取模运算，从而提升性能</B>
   * @Author zm
   * @Date 2022年06月01日 09:06:57
   * @Param [h, length]
   * @return int
   * 注意：这里要确保只有一个地方能够调用这个方法。如果多个地方调用这个方法这里就不合适了，因为 变量 TWO_POWER_FLAG 只会被初始化一次。
   **/
  public static int indexForByProcessor(int h, int length) {
    if(null == TWO_POWER_FLAG_BY_PROCESSOR){
      //
      if(true == isTwoPower(length)){
          TWO_POWER_FLAG_BY_PROCESSOR = true;
      }else{
          TWO_POWER_FLAG_BY_PROCESSOR = false;
      }
    }
    if(true  == TWO_POWER_FLAG_BY_PROCESSOR){
      // 使用按位与获取下标；2022-09-13 14:20:28
      return h & (length - 1);
    }
    // 当length不是2的幂次方的时候，使用取模获取下标。2022-09-13 14:21:07
    return h % length;
  }

  public static int indexForByIoThread(int h, int length) {
    if(null == TWO_POWER_FLAG_BY_IOTHREADER){
      //
      if(true == isTwoPower(length)){
          TWO_POWER_FLAG_BY_IOTHREADER = true;
      }else{
          TWO_POWER_FLAG_BY_IOTHREADER = false;
      }
    }
    if(true  == TWO_POWER_FLAG_BY_IOTHREADER){
      // 使用按位与获取下标；2022-09-13 14:20:28
      return h & (length - 1);
    }
    // 当length不是2的幂次方的时候，使用取模获取下标。2022-09-13 14:21:07
    return h % length;
  }

  /**
   * <B>方法名称：isTwoPower</B>
   * <B>概要说明：判断是不是2的幂次方</B>
   * @Author zm
   * @Date 2022年09月13日 14:09:43
   * @Param [n]
   * @return boolean
   **/
  public static boolean isTwoPower(int n) {
    return n>0 && (n&(n-1)) == 0;
  }

}
