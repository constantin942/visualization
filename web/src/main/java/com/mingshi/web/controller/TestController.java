package com.mingshi.web.controller;

import com.mingshi.skyflying.common.utils.DbUtil;
import com.mingshi.skyflying.common.utils.RedisPoolUtil;
import com.mingshi.skyflying.impl.SegmentConsumeServiceImpl;
import com.mingshi.skyflying.kafka.producer.AiitKafkaProducer;
import com.mingshi.skyflying.service.SegmentConsumerService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import java.time.Instant;

/**
 * @Author zhaoming
 * @Description 不需要登录校验的接口写这里面
 * @Date 15:28 2020/2/2
 * @Param
 * @return
 **/
@RestController
@Slf4j
@RequestMapping("/api/test")
public class TestController {
  @Value("${spring.datasource.url}")
  private String url;
  // @Value("${doc_dir_path}")
  private String docDirPath;

  @Autowired
  DriverManagerDataSource driverManagerDataSource;

  @Resource
  private DbUtil dbUtil;
  @Resource
  private RedisPoolUtil redisPoolUtil;
  @Resource
  private AiitKafkaProducer aiitKafkaProducer;

  @Resource
  private SegmentConsumeServiceImpl segmentConsumerService;
  private String topic = "zm-test-topic-02";
  private Instant nowCpuMemory = Instant.now();

  /**
   * <B>方法名称：test</B>
   * <B>概要说明：生成数据库设计文档</B>
   * @Author zm
   * @Date 2022年08月23日 14:08:38
   * @Param []
   * @return void
   **/
  @GetMapping(value = "/createDbDocument")
  public void createDbDocument() {
    String dataBaseName = url.split("/")[3].split("useUnicode")[0].replace("?","");
    dbUtil.createWord(dataBaseName, docDirPath);
  }

  @GetMapping(value = "/sendMsg")
  public void testSendMsgToKafka(String topic){
    for (int i = 0; i < 1; i++) {
    // for (int i = 0; i < 1000 * 10000; i++) {
      System.out.println("开始发送第" + (i + 1) + "条消息：" + i);
      aiitKafkaProducer.send(topic,i + "");
    }
  }
  /**
   * <B>方法名称：test</B>
   * <B>概要说明：测试部署了skywalking探针对业务系统的影响有多少，从请求时间、CPU占用率、内存使用率三个维度来考量</B>
   *
   * @return com.mingshi.skyflying.common.domain.CommonResponse
   * @Author zm
   * @Date 2022年08月04日 16:08:29
   * @Param []
   **/
  // @ResponseBody
  // @GetMapping(value = "/test")
  // public CommonResponse test() {
  //   HashSet<String> postHashSet = new HashSet<>();
  //   HashSet<String> getHashSet = new HashSet<>();
  //   getHashSet.add("/firewall/getAllFirewallExchangeMatch");
  //   getHashSet.add("/firewall/getHighRiskPort?pageNum=1&pageSize=10");
  //   getHashSet.add("/firewall/getIntranetIp?pageNum=1&pageSize=10");
  //   getHashSet.add("/firewall/getIntranetIpMatchTimes?intranetIp=&pageNum=1&pageSize=10");
  //   getHashSet.add("/firewall/firewallRuleOptimizationSuggestionsList?orderId=&pageNum=1&pageSize=10");
  //   getHashSet.add("/firewall/getVisitedDesinationIpCount?firewallIp=");
  //   getHashSet.add("/firewall/getHighDangerousPortCount?firewallIp=");
  //   getHashSet.add("/firewall/getVisitSourceIpCount?firewallIp=");
  //   postHashSet.add("/firewall/userList");
  //
  //   getHashSet.add("/firewall/getAllFirewallExchangeMatch");
  //   getHashSet.add("/firewall/getHighRiskPort?pageNum=1&pageSize=10");
  //   getHashSet.add("/firewall/getIntranetIp?pageNum=1&pageSize=10");
  //   getHashSet.add("/firewall/getIntranetIpMatchTimes?intranetIp=&pageNum=1&pageSize=10");
  //   getHashSet.add("/firewall/firewallRuleOptimizationSuggestionsList?orderId=&pageNum=1&pageSize=10");
  //   getHashSet.add("/firewall/getVisitedDesinationIpCount?firewallIp=");
  //   getHashSet.add("/firewall/getHighDangerousPortCount?firewallIp=");
  //   getHashSet.add("/firewall/getVisitSourceIpCount?firewallIp=");
  //   postHashSet.add("/firewall/userList");
  //
  //   log.info("开始执行#SkyflyingController#test");
  //
  //   Map<String, String> map24CpuMemory = new HashMap();
  //   Map<String, String> map25CpuMemory = new HashMap();
  //
  //   try {
  //     while (true) {
  //       String prx24 = "http://10.0.107.24:18082";
  //       String prx25 = "http://10.0.107.25:18082";
  //       for (String url : getHashSet) {
  //         String item = DateTimeUtil.formatWithDATE_TIME_FULL(new Date());
  //         doGet(prx24 + url, "Get_", item);
  //         doGet(prx25 + url, "no_skywalking_Get_", item);
  //       }
  //       for (String url : postHashSet) {
  //         String item = DateTimeUtil.formatWithDATE_TIME_FULL(new Date());
  //         doPost(prx24 + url, "Post_", item);
  //         doPost(prx25 + url, "no_skywalking_Post_", item);
  //       }
  //       // 获取内存和CPU使用情况
  //       getCpuAndMemoryUsedDetail(map24CpuMemory, map25CpuMemory);
  //     }
  //   } catch (Exception e) {
  //     log.error("# TestController.test() # 测试部署了skywalking探针对业务系统的影响有多少时，出现了异常。", e);
  //     return null;
  //   }
  // }

  /**
   * <B>方法名称：getResult</B>
   * <B>概要说明：获取统计结果</B>
   *
   * @return com.mingshi.skyflying.common.domain.CommonResponse
   * @Author zm
   * @Date 2022年08月05日 10:08:43
   * @Param []
   **/
  // @ResponseBody
  // @GetMapping(value = "/getResult")
  // public ServerResponse<String> getResult() {
  //   HashSet<String> postHashSet = new HashSet<>();
  //   HashSet<String> getHashSet = new HashSet<>();
  //   getHashSet.add("/firewall/getAllFirewallExchangeMatch");
  //   getHashSet.add("/firewall/getHighRiskPort?pageNum=1&pageSize=10");
  //   getHashSet.add("/firewall/getIntranetIp?pageNum=1&pageSize=10");
  //   getHashSet.add("/firewall/getIntranetIpMatchTimes?intranetIp=&pageNum=1&pageSize=10");
  //   getHashSet.add("/firewall/firewallRuleOptimizationSuggestionsList?orderId=&pageNum=1&pageSize=10");
  //   getHashSet.add("/firewall/getVisitedDesinationIpCount?firewallIp=");
  //   getHashSet.add("/firewall/getHighDangerousPortCount?firewallIp=");
  //   getHashSet.add("/firewall/getVisitSourceIpCount?firewallIp=");
  //   postHashSet.add("/firewall/userList");
  //
  //   getHashSet.add("/firewall/getAllFirewallExchangeMatch");
  //   getHashSet.add("/firewall/getHighRiskPort?pageNum=1&pageSize=10");
  //   getHashSet.add("/firewall/getIntranetIp?pageNum=1&pageSize=10");
  //   getHashSet.add("/firewall/getIntranetIpMatchTimes?intranetIp=&pageNum=1&pageSize=10");
  //   getHashSet.add("/firewall/firewallRuleOptimizationSuggestionsList?orderId=&pageNum=1&pageSize=10");
  //   getHashSet.add("/firewall/getVisitedDesinationIpCount?firewallIp=");
  //   getHashSet.add("/firewall/getHighDangerousPortCount?firewallIp=");
  //   getHashSet.add("/firewall/getVisitSourceIpCount?firewallIp=");
  //   postHashSet.add("/firewall/userList");
  //
  //   String prx24 = "http://10.0.107.24:18082";
  //   String prx25 = "http://10.0.107.25:18082";
  //
  //   log.info("开始执行#SkyflyingController#getResult()");
  //
  //   Map<String/* url */, Map<Integer/* 总的访问次数 */, Integer/* 平均访问时间 */>> getMap24 = new HashMap<>(Const.NUMBER_EIGHT);
  //   Map<String/* url */, Map<Integer/* 总的访问次数 */, Integer/* 平均访问时间 */>> getMap25 = new HashMap<>(Const.NUMBER_EIGHT);
  //
  //   Map<String/* url */, Map<Integer/* 总的访问次数 */, Integer/* 平均访问时间 */>> postMap24 = new HashMap<>(Const.NUMBER_EIGHT);
  //   Map<String/* url */, Map<Integer/* 总的访问次数 */, Integer/* 平均访问时间 */>> postMap25 = new HashMap<>(Const.NUMBER_EIGHT);
  //
  //   Map<String/* cpuMemory */, Map<String/* 总的访问次数 */, Double/* 平均负载 */>> cpuMemoryMap24 = new HashMap<>(Const.NUMBER_EIGHT);
  //   Map<String/* cpuMemory */, Map<String/* 总的访问次数 */, Double/* 平均负载 */>> cpuMemoryMap25 = new HashMap<>(Const.NUMBER_EIGHT);
  //
  //   HashMap<Object, Object> statisticsMap = new HashMap<>(Const.NUMBER_EIGHT);
  //   statisticsMap.put("24_Get", getMap24);
  //   statisticsMap.put("25_Get", getMap25);
  //   statisticsMap.put("24_Post", postMap24);
  //   statisticsMap.put("25_Post", postMap25);
  //   statisticsMap.put("24_cpuMemory", cpuMemoryMap24);
  //   statisticsMap.put("25_cpuMemory", cpuMemoryMap25);
  //   try {
  //     for (String url : getHashSet) {
  //       String key1 = "Get_" + prx24 + url;
  //       Map<Object, Object> hgetall1 = redisPoolUtil.hgetall(key1);
  //       getAverage(getMap24, key1, hgetall1);
  //
  //       String key2 = "no_skywalking_Get_" + prx25 + url;
  //       Map<Object, Object> hgetall2 = redisPoolUtil.hgetall(key2);
  //       getAverage(getMap25, key2, hgetall2);
  //     }
  //     for (String url : postHashSet) {
  //       String key1 = "Post_" + prx24 + url;
  //       Map<Object, Object> hgetall1 = redisPoolUtil.hgetall(key1);
  //       getAverage(postMap24, key1, hgetall1);
  //
  //       String key2 = "no_skywalking_Post_" + prx25 + url;
  //       Map<Object, Object> hgetall2 = redisPoolUtil.hgetall(key2);
  //       getAverage(postMap25, key2, hgetall2);
  //     }
  //
  //
  //     String flag24 = "cpu_memory_used_detail_skywalking_agent_24";
  //     Map<Object, Object> cpu_memory_used_detail_skywalking_agent_24 = redisPoolUtil.hgetall(flag24);
  //     getCpuAndMemoryLoad(cpu_memory_used_detail_skywalking_agent_24, flag24, cpuMemoryMap24);
  //
  //     String flag25 = "cpu_memory_used_detail_no_skywalking_agent_25";
  //     Map<Object, Object> cpu_memory_used_detail_no_skywalking_agent_25 = redisPoolUtil.hgetall(flag25);
  //     getCpuAndMemoryLoad(cpu_memory_used_detail_no_skywalking_agent_25, flag25, cpuMemoryMap25);
  //
  //
  //     ServerResponse<String> serverResponse = new ServerResponse<>();
  //     serverResponse.setData(JsonUtil.obj2String(statisticsMap));
  //     return serverResponse;
  //   } catch (Exception e) {
  //     log.error("# TestController.test() # 测试部署了skywalking探针对业务系统的影响有多少时，出现了异常。", e);
  //     return null;
  //   }
  // }

  // private void getCpuAndMemoryLoad(Map<Object, Object> cpuMemoryUsedDetailSkywalkingAgent, String flag, Map<String, Map<String, Double>> cpuMemoryMap) {
  //   if (null != cpuMemoryUsedDetailSkywalkingAgent && 0 < cpuMemoryUsedDetailSkywalkingAgent.size()) {
  //     Integer size = cpuMemoryUsedDetailSkywalkingAgent.size();
  //     Double countCpuLoad = 0d;
  //     Double countMemoryLoad = 0d;
  //     Iterator<Object> iterator = cpuMemoryUsedDetailSkywalkingAgent.keySet().iterator();
  //     while (iterator.hasNext()) {
  //       Object key = iterator.next();
  //       Object value = cpuMemoryUsedDetailSkywalkingAgent.get(key);
  //       String[] split = String.valueOf(value).split("\n");
  //       String cpuLoad = split[0].split(":")[1].trim();
  //       countCpuLoad += Double.valueOf(cpuLoad);
  //       String memoryLoad = split[1].split(":")[1].split("GB")[0].trim();
  //       countMemoryLoad += Double.valueOf(memoryLoad);
  //     }
  //     Map<String, Double> map = new HashMap<>(Const.NUMBER_EIGHT);
  //     map.put("allSamples", size.doubleValue());
  //     map.put("cpuAverageSamples", countCpuLoad / size);
  //     map.put("memoryAverageSamples", countMemoryLoad / size);
  //     cpuMemoryMap.put(flag,map);
  //   }
  // }

  /**
   * <B>方法名称：getAverage</B>
   * <B>概要说明：计算每个url平均访问时间</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年08月05日 10:08:35
   * @Param [returnMap, url, hgetall]
   **/
  // private void getAverage(Map<String, Map<Integer, Integer>> returnMap, String url, Map<Object, Object> hgetall) {
  //   if (null != hgetall && 0 < hgetall.size()) {
  //     int size = hgetall.size();
  //     Iterator<Object> iterator = hgetall.keySet().iterator();
  //     int count = 0;
  //     while (iterator.hasNext()) {
  //       Object key = iterator.next();
  //       Object o = hgetall.get(key);
  //       int usedTime = Integer.parseInt(String.valueOf(o));
  //       count += usedTime;
  //     }
  //     Map<Integer, Integer> map = new HashMap<>(Const.NUMBER_EIGHT);
  //     map.put(size, count / size);
  //     returnMap.put(url, map);
  //   }
  // }

  /**
   * <B>方法名称：getCpuAndMemoryUsedDetail</B>
   * <B>概要说明：获取内存和CPU使用情况</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年08月04日 17:08:36
   * @Param []
   **/
  // private void getCpuAndMemoryUsedDetail(Map<String, String> map24CpuMemory, Map<String, String> map25CpuMemory) {
  //   if (LinuxStateForShellConfig.getAtomicBoolean()) {
  //     Map<String, String> result1 = LinuxStateForShellUtil.runDistanceShell(LinuxStateForShellConfig.getSession24());
  //     String item = DateTimeUtil.formatWithDateTimeFull(new Date());
  //     String x = LinuxStateForShellUtil.disposeResultMessage(result1);
  //     if (StringUtil.isNotBlank(x)) {
  //       Map<String, String> result2 = LinuxStateForShellUtil.runDistanceShell(LinuxStateForShellConfig.getSession25());
  //       String x1 = LinuxStateForShellUtil.disposeResultMessage(result2);
  //       if (StringUtil.isNotBlank(x1)) {
  //         map24CpuMemory.put(item, x);
  //         map25CpuMemory.put(item, x1);
  //         if (20 < DateTimeUtil.getSecond(nowCpuMemory)) {
  //           log.info("########## cpu使用情况，开始本地批量刷新。map24.size() = 【{}】。", map24CpuMemory.size());
  //           redisPoolUtil.hsetBatch("cpu_memory_used_detail_skywalking_agent_24", map24CpuMemory);
  //           map24CpuMemory.clear();
  //           log.info("########## cpu使用情况，本次批量刷新结束。map24.size() = 【{}】。", map24CpuMemory.size());
  //
  //           log.info("########## cpu使用情况，开始本地批量刷新。map25.size() = 【{}】。", map25CpuMemory.size());
  //           redisPoolUtil.hsetBatch("cpu_memory_used_detail_no_skywalking_agent_25", map25CpuMemory);
  //           map25CpuMemory.clear();
  //           log.info("########## cpu使用情况，本次批量刷新结束。map25.size() = 【{}】。", map25CpuMemory.size());
  //
  //           nowCpuMemory = Instant.now();
  //         }
  //       }
  //     }
  //   }
  // }

  /**
   * <B>方法名称：doGet</B>
   * <B>概要说明：获取get请求执行时间</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年08月04日 17:08:09
   * @Param [url, flag, item]
   **/
  // private void doGet(String url, String flag, String item) {
  //   Instant start = Instant.now();
  //   HttpUtil.get(url);
  //   Long timeMillis = DateTimeUtil.getTimeMillis(start);
  //   redisPoolUtil.hset(flag + url, item, timeMillis.toString());
  // }

  /**
   * <B>方法名称：doPost</B>
   * <B>概要说明：获取post请求执行时间</B>
   *
   * @return
   * @Author zm
   * @Date 2022年08月04日 17:08:27
   * @Param
   **/
  // private void doPost(String url, String flag, String item) {
  //   Instant start = Instant.now();
  //   HttpUtil.post(url);
  //   Long timeMillis = DateTimeUtil.getTimeMillis(start);
  //   redisPoolUtil.hset(flag + url, item, timeMillis.toString());
  // }

  @GetMapping("tableRuleEnable")
  public void tableRuleEnable() {
    segmentConsumerService.getEnableRule("table");
  }

}
