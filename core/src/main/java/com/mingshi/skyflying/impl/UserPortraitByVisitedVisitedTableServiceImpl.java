package com.mingshi.skyflying.impl;

import com.mingshi.skyflying.anomaly_detection.singleton.AnomylyDetectionSingletonByVisitedTableEveryday;
import com.mingshi.skyflying.constant.Const;
import com.mingshi.skyflying.dao.MsSegmentDetailDao;
import com.mingshi.skyflying.dao.UserPortraitByVisitedTableEverydayMapper;
import com.mingshi.skyflying.dao.UserPortraitByVisitedTimeMapper;
import com.mingshi.skyflying.domain.InstanceTable;
import com.mingshi.skyflying.domain.MsSegmentDetailDo;
import com.mingshi.skyflying.domain.UserPortraitByVisitedTableEverydayDo;
import com.mingshi.skyflying.response.ServerResponse;
import com.mingshi.skyflying.service.UserPortraitByVisitedTableService;
import com.mingshi.skyflying.utils.DateTimeUtil;
import com.mingshi.skyflying.utils.JsonUtil;
import com.mingshi.skyflying.utils.StringUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

// TODO：与弘毅讨论下这个异常检测该怎么实现？
// 2022-06-06 17:02:50
// 异常检测规则有：
// 1. 若某用户通常白天访问数据，则夜间为异常；
// 2. 某用户访问从未访问过的表；
// 3. 访问频率、访问量激增；
// 李老师说，这个访问量激增就暂时不考虑了，按照李老师的设想，访问量指的是用户每天访问数据库的数据的量，
// 比如，访问某个表，select多条数据。这个实现有点复杂，所以李老师说就暂不实现了。2022-06-10 09:25:41
// 访问频率指的是：
//     先对历史数据统计每个用户对每个表的访问次数，此时生成一个基于访问次数的用户画像；
//     然后把用户每天对每个表的访问次数累加起来，并定时存入到数据库中保存。当每天凌晨12点时，
//         计算下当天这个用户对每个表的总访问次数，并把这些访问次数与历史访问次数做对比，若是高出某个闸值，那么就给出一个告警信息。
//     注：访问频率这个功能已经集成到第二条检测功能中了。当之前基于历史数据生成一个用户画像之后，新来的访问链路信息会在用户画像中寻找是否访问过这个表？
//         如果访问过，则继续在当天统计的基础上继续累加；

/**
 * <B>方法名称：UserPortraitByTimeServiceImpl</B>
 * <B>概要说明：根据用户访问系统的时间，来计算其时间维度的画像。</B>
 * 工作流程：
 * a. 首先根据历史数据，初步生成用户的访问时间维度的画像，并存储到数据库中；
 * b. 项目启动的时候，从数据库中加载访问时间维度的画像到jvm本地内存中；
 * c. 消费者拿到用户访问系统的链路信息之后，
 * c1. 如果该链路中没有用户名，则跳过异常检测。由后续的定时任务来执行异常检测（后续更新了用户名信息），其具体实施步骤如下：
 * c11. 从ms_segment_detail表中，将用户名不为空且未检测过的基于访问时间的记录查询出来，然后走异常判断逻辑。
 * c2. 如果该链路信息中有用户名，那么就进行访问时间维度的异常检测。同时将该记录标识为已进行了过了基于访问时间的异常检测；
 * c3. 如果发生异常，则将异常信息保存到数据库中；
 * c4. 如果没有发生异常，则将用户在当前时间段内的访问次数累加1。然后定时将累加后的数据更新到MySQL数据库中；
 * 目前这个功能已实现，能够满足：a. 实时的检测异常行为；b. 分钟级别更新用户画像信息；
 *
 * @Author zm
 * @Date 2022年06月07日 14:06:43
 * @Param
 * @return
 **/
@Slf4j
@Service("userPortraitByVisitedTableService")
public class UserPortraitByVisitedVisitedTableServiceImpl implements UserPortraitByVisitedTableService {

  @Resource
  private MsSegmentDetailDao msSegmentDetailDao;
  @Resource
  private UserPortraitByVisitedTableEverydayMapper userPortraitByVisitedTableEverydayMapper;
  @Resource
  private UserPortraitByVisitedTimeMapper userPortraitByVisitedTimeMapper;

  @Override
  public ServerResponse<String> createUserPortraitByVisitedTableEveryday() {
    Map<String/* 用户名 */,Map<String/* 数据库名 */,Map<String/* 访问过的表 */, Map<String/* 访问日期，以天为单位 */, Map<String,/* 数据库操作类型：insert、delete、update、select */ Integer/* 访问次数 */>>>>> statisticsMap = new HashMap<>();

    // 统计用户每天访问过的表的访问次数
    statisticsEverydayVistedTableCount(statisticsMap);

    // 批量插入用户的访问过的表的统计数据
    batchInsertUserPortraitByVisitedTable(statisticsMap);

    return ServerResponse.createBySuccess();
  }

  /**
   * <B>方法名称：getUserPortraitByVisitedTableEveryday</B>
   * <B>概要说明：获取用户基于每天访问过的数据库表次数的画像</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月13日 09:06:52
   * @Param [userName, visitedTable, visitedTime, pageNo, pageSize]
   **/
  @Override
  public ServerResponse<String> getUserPortraitByVisitedTableEveryday(String userName, String visitedTable, String visitedDate, String visitedDbInstance,Integer pageNo, Integer pageSize) {
    Map<String, Object> queryMap = new HashMap<>();
    if (StringUtil.isNotBlank(userName)) {
      queryMap.put("userName", userName);
    }
    if (StringUtil.isNotBlank(visitedTable)) {
      queryMap.put("visitedTable", visitedTable);
    }
    if (StringUtil.isNotBlank(visitedDate)) {
      queryMap.put("visitedDate", visitedDate);
    }
    if (StringUtil.isNotBlank(visitedDbInstance)) {
      queryMap.put("visitedDbInstance", visitedDbInstance);
    }
    if (null == pageNo) {
      pageNo = 1;
    }
    if (null == pageSize) {
      pageSize = 10;
    }
    queryMap.put("pageNo", (pageNo - 1) * pageSize);
    queryMap.put("pageSize", pageSize);

    List<UserPortraitByVisitedTableEverydayDo> userPortraitByVisitedTableEverydayDos = userPortraitByVisitedTableEverydayMapper.selectByUserNameAndVisitedTableAndVisitedDate(queryMap);


    Integer count = userPortraitByVisitedTableEverydayMapper.selectByUserNameAndVisitedTableAndVisitedDateCount(queryMap);
    Map<String, Object> context = new HashMap<>();
    context.put("rows", JsonUtil.obj2String(userPortraitByVisitedTableEverydayDos));
    context.put("total", count);

    log.info("# UserPortraitByVisitedVisitedTableServiceImpl.getUserPortraitByVisitedTableEveryday() # 根据条件【{}】在数据库中查询到了【{}】条数据。", JsonUtil.obj2String(queryMap), userPortraitByVisitedTableEverydayDos.size());
    ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    bySuccess.setData(JsonUtil.obj2String(context));
    return bySuccess;
  }

  /**
   * <B>方法名称：getAllUserNameUserPortraitByVisitedTableEveryday</B>
   * <B>概要说明：获取所有的访问过的表</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月13日 10:06:00
   * @Param []
   **/
  @Override
  public ServerResponse<String> getAllVisitedTablePortraitByVisitedTableEveryday() {
    HashSet<InstanceTable> hashSet=new HashSet<>();
    List<InstanceTable> visitedTableList = userPortraitByVisitedTableEverydayMapper.selectAllVisitedTable();

    for (InstanceTable table : visitedTableList) {
      if (table.getMsTableName().contains(",")) {
        String[] split = table.getMsTableName().split(",");
        for (String s : split) {
          hashSet.add(new InstanceTable(table.getDbInstance(),s));
        }
      } else {
        hashSet.add(new InstanceTable(table.getDbInstance(),table.getMsTableName()));
      }
    }

    ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    bySuccess.setData(JsonUtil.obj2String(hashSet));
    return bySuccess;
  }

  /**
   * <B>方法名称：updateUserPortraitByVisitedTableEverydayRule</B>
   * <B>概要说明：更新用户访问过的表的画像规则状态。isDelete=0，启用这个规则；isDelete=1，禁用这个规则</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月16日 14:06:35
   * @Param [ruleId, isDelete]
   **/
  @Override
  public ServerResponse<String> updateUserPortraitByVisitedTableEverydayRule(Integer ruleId, Integer isDelete) {
    log.info("开始执行 # UserPortraitByVisitedVisitedTableServiceImpl.updateUserPortraitByVisitedTableEverydayRule() # 更新用户访问过的表的画像规则启用状态。 ");
    if (!isDelete.equals(Const.IS_DELETE_ZERO) && !isDelete.equals(Const.IS_DELETE_ONE)) {
      return ServerResponse.createByErrorMessage("参数非法：是否启用的参数isDelete应该是0或者1.", "");
    }

    // 先根据规则id在数据库中找到这条规则；2022-06-16 14:44:21
    UserPortraitByVisitedTableEverydayDo userPortraitByVisitedTableEverydayDo = userPortraitByVisitedTableEverydayMapper.selectByPrimaryKey(ruleId);
    if (null == userPortraitByVisitedTableEverydayDo) {
      return ServerResponse.createByErrorMessage("参数非法：规则id在数据库中不存在.", "");
    }

    Integer isDelete1 = userPortraitByVisitedTableEverydayDo.getIsDelete();
    if(isDelete1.equals(isDelete)){
      return ServerResponse.createBySuccess();
    }
    // 设置规则启用/禁用的状态；2022-06-16 15:20:09
    userPortraitByVisitedTableEverydayDo.setIsDelete(isDelete);

    // TODO: 2022/6/16 这里如果要禁用或者启用一条规则，要同时把本地内存中存储的规则执行相同的操作。
    // 比如，一条规则之前都是在运行状态，现在要禁用了，那么需要在内存里删除这条规则。
    // 如果一条规则本来已经禁用了，现在要启用这条规则，那么需要将这条规则从数据库中加载到本地内存中。
    // 更新本地内存；2022-06-16 14:49:29
    ServerResponse<String> response = updateLocalMemoryAndDbByUserPortraitByVisitedTable(userPortraitByVisitedTableEverydayDo, isDelete);
    log.info("执行完毕 # UserPortraitByVisitedVisitedTableServiceImpl.updateUserPortraitByVisitedTableEverydayRule() # 更新用户访问过的表的画像规则启用状态。 ");
    return response;
  }

  /**
   * <B>方法名称：addUserPortraitByVisitedTableEverydayRule</B>
   * <B>概要说明：增加用户访问过的表的画像规则</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月16日 16:41:35
   * @Param [ruleId, isDelete]
   **/
  @Override
  public ServerResponse<String> addUserPortraitByVisitedTableEverydayRule(String userName, String visitedTable, Integer visitedCount, String visitedDate, String dbType) {
    if (StringUtil.isBlank(userName)) {
      return ServerResponse.createByErrorMessage("参数 userName 不能为空。", "");
    }
    if (StringUtil.isBlank(visitedTable)) {
      return ServerResponse.createByErrorMessage("参数 visitedTable 不能为空。", "");
    }
    if (null == visitedCount || 0 > visitedCount) {
      return ServerResponse.createByErrorMessage("参数 visitedCount 不能为空或不能为负数。", "");
    }
    if (StringUtil.isBlank(visitedDate)) {
      return ServerResponse.createByErrorMessage("参数 visitedDate 不能为空。", "");
    }
    if (StringUtil.isBlank(dbType)) {
      return ServerResponse.createByErrorMessage("参数 dbType 不能为空。", "");
    }
    UserPortraitByVisitedTableEverydayDo userPortraitByVisitedTableEverydayDo = new UserPortraitByVisitedTableEverydayDo();
    userPortraitByVisitedTableEverydayDo.setUserName(userName);
    userPortraitByVisitedTableEverydayDo.setVisitedTable(visitedTable);
    userPortraitByVisitedTableEverydayDo.setVisitedCount(visitedCount);
    userPortraitByVisitedTableEverydayDo.setVisitedDate(visitedDate);
    userPortraitByVisitedTableEverydayDo.setDbType(dbType);
    int insertResult = userPortraitByVisitedTableEverydayMapper.insertSelective(userPortraitByVisitedTableEverydayDo);
    if (1 != insertResult) {
      log.error(" # UserPortraitByVisitedVisitedTableServiceImpl.addUserPortraitByVisitedTableEverydayRule() # 增加用户访问过的表的规则时，插入到表中失败。");
      return ServerResponse.createByErrorMessage("数据库插入操作失败。", "");
    }

    // 将规则加入到本地内存中，这里的做法只适用于单实例部署，如果有定时任务定时将数据库中的规则加载到本地内存，也适用。2022-06-16 16:51:04
    updateLocalMemoryUserPortraitByVisitedTableEveryday(userPortraitByVisitedTableEverydayDo);

    return ServerResponse.createBySuccess();
  }

  /**
   * <B>方法名称：updateLocalMemoryAndDb</B>
   * <B>概要说明：当前端禁用或启用一条规则时，要把该动作同步到本地内存中。</B>
   * 注：当在前端触发禁启用一条规则时，当前的做法近适用于单实例部署该系统。
   * 当多实例部署该系统时，有多种实现方式，将数据库中的规则与本地内存中保持一致。
   * 方式一：使用ETCD或者Zookeeper：当前端发送一个规则变更请求后，这个请求会打到后端其中一个实例上。这个实例可以先进行数据库和本地内存同步规则的动作，然后对ETCD或者Zookeeper上的某个路径执行更新操作。
   * 在实例中创建一个观察者Watcher（这是观察者设计模式），这个Watcher会监控某一路径是否有变更，如果有变更，则从数据库中重新加载所有的规则到本地内存。
   * 方式二：使用RocketMQ：在Broker端创建一个topic，然后在每个实例中创建对应的消费者，这些消费者属于同一个消费者组，且这个消费者组设置为广播模式。也可以为每一个消费者设置为单独的消费者组。
   * 当其中一个实例接收到前端的请求之后，可以先把这个请求发送到RocketMQ中，接着在数据库和本地内存中同步这个规则。当其它的消费者订阅了这个topic并且接收到这条消息之后，在自己的实例中同步（数据库和本地内存）这个规则。
   * 方式三：使用Kafka：在Broker端创建一个topic，然后在每个实例中创建对应的消费者。默认情况下，属于同一个消费者组的消费者会消费到不同的消息。如果想让每个消费者都消费到每一条消息，那么在Kafka中只能将各个消费者设置为不同的消费者组。
   * 具体的设置方式是@KafkaListener(topics = {"topic"}, groupId = "consumerGroup-" + "#{T(java.util.UUID).randomUUID()}")。当其中一个实例接收到前端的请求之后，可以先把这个请求发送到RocketMQ中，
   * 接着在数据库和本地内存中同步这个规则。当其它的消费者订阅了这个topic并且接收到这条消息之后，在自己的实例中同步（数据库和本地内存）这个规则。
   * 上述无论哪种方案都有自己的优缺点。当某种方式执行失败时，需要有一个兜底方案。
   * 兜底方案：在实例中，启动一个定时任务。每隔多长时间，比如每间隔1分钟，就把数据库中的规则重新同步到本地内存中。这样无论启动多少个实例，当规则变化后，最多1分钟就可以感知到规则变化了。当有了这种兜底方案之后，可以不用方式一、二、三。
   * 使用方式一、二、三的优点是：当规则有变更后，可以立马感知到。使用方式一、二、三的缺点是：当执行失败时，还得使用兜底方案来保证数据库和本地内存中数据的一致性。
   * 仅仅使用兜底方案也可以，兜底方案的优点是：实现简单，每隔一定的时间从数据库中加载规则到本地内存。
   * 兜底方案的缺点是：
   * a. 规则变更有一定的延迟，不能立马感知到。这个延迟取决于间隔的时间；
   * b. 当数据库压力比较大的时候，如果定时任务间隔时间太短，会对数据库造成较大的访问压力；
   * c. 当从数据库中同步规则到本地内存期间，这段时间是不能进行本地统计的。
   * 作者：骨架弯弯99
   * 链接：https://wenku.baidu.com/view/0b1eaadf971ea76e58fafab069dc5022abea465f.html
   * 来源：百度文库
   * 著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。
   *
   * @return void
   * @Author zm
   * @Date 2022年06月16日 14:06:05
   * @Param [userPortraitByVisitedTableEverydayDo]
   **/
  private ServerResponse<String> updateLocalMemoryAndDbByUserPortraitByVisitedTable(UserPortraitByVisitedTableEverydayDo userPortraitByVisitedTableEverydayDo, Integer isDelete) {
    if (isDelete.equals(Const.IS_DELETE_ONE)) {
      // 禁用这条规则；2022-06-16 14:55:51
      return updateLocalMemoryNoEnableByUserPortraitByVisitedTable(userPortraitByVisitedTableEverydayDo);
    }
    if (isDelete.equals(Const.IS_DELETE_ZERO)) {
      // 启用这条规则；2022-06-16 14:55:51
      return updateLocalMemoryEnableByUserPortraitByVisitedTable(userPortraitByVisitedTableEverydayDo);
    }

    return null;
  }


  /**
   * <B>方法名称：updateLocalMemoryEnableByUserPortraitByVisitedTable</B>
   * <B>概要说明：启用这条规则；先更新数据库，然后把这条规则加载到内存中。</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月16日 14:06:04
   * @Param [userPortraitByVisitedTableEverydayDo]
   **/
  private ServerResponse<String> updateLocalMemoryEnableByUserPortraitByVisitedTable(UserPortraitByVisitedTableEverydayDo userPortraitByVisitedTableEverydayDo) {
    try {
      int updateResult = userPortraitByVisitedTableEverydayMapper.updateByPrimaryKeySelective(userPortraitByVisitedTableEverydayDo);
      if (1 != updateResult) {
        log.error(" # UserPortraitByVisitedVisitedTableServiceImpl.updateLocalMemoryEnableByUserPortraitByVisitedTable() # 把禁用这条规则的状态更新到数据库中失败。");
        return ServerResponse.createByErrorMessage("更新数据库操作失败", "");
      }

      updateLocalMemoryUserPortraitByVisitedTableEveryday(userPortraitByVisitedTableEverydayDo);
    } catch (Exception e) {
      log.error("# UserPortraitByVisitedVisitedTableServiceImpl.updateLocalMemoryEnableByUserPortraitByVisitedTable() # 启用用户访问过的规则时，出现了异常。", e);
    }

    return ServerResponse.createBySuccess();
  }

  /**
   * <B>方法名称：updateLocalMemoryUserPortraitByVisitedTableEveryday</B>
   * <B>概要说明：更新本地内存</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月16日 16:06:57
   * @Param [userPortraitByVisitedTableEverydayDo]
   **/
  private void updateLocalMemoryUserPortraitByVisitedTableEveryday(UserPortraitByVisitedTableEverydayDo userPortraitByVisitedTableEverydayDo) {
    Boolean userPortraitByVisitedTableEnable = AnomylyDetectionSingletonByVisitedTableEveryday.getUserPortraitByVisitedTableEnable();
    if(false == userPortraitByVisitedTableEnable){
      return;
    }
    Map<String/* 用户名 */,
      Map<String/* 访问过的表 */,
        Map<String/* 访问日期，以天为单位 */,
          Map<String,/* 数据库操作类型：insert、delete、update、select */
            Integer/* 访问次数 */>>>> userPortraitByVisitedTableMap = AnomylyDetectionSingletonByVisitedTableEveryday.getUserPortraitByVisitedTableMap();
    String userName = userPortraitByVisitedTableEverydayDo.getUserName();
    String visitedTable = userPortraitByVisitedTableEverydayDo.getVisitedTable();
    String visitedDate = userPortraitByVisitedTableEverydayDo.getVisitedDate();
    String dbType = userPortraitByVisitedTableEverydayDo.getDbType();
    Integer visitedCount = userPortraitByVisitedTableEverydayDo.getVisitedCount();
    Map<String, Integer> dbtypeCountMap = null;
    Map<String, Map<String, Map<String, Integer>>> visitedTableDateDbtypeCountMap = userPortraitByVisitedTableMap.get(userName);
    if (null == visitedTableDateDbtypeCountMap) {
      visitedTableDateDbtypeCountMap = new ConcurrentHashMap<>();
      userPortraitByVisitedTableMap.put(userName, visitedTableDateDbtypeCountMap);
    }
    Map<String, Map<String, Integer>> dateDbtypeCountMap = visitedTableDateDbtypeCountMap.get(visitedTable);
    if (null == dateDbtypeCountMap) {
      dateDbtypeCountMap = new ConcurrentHashMap<>();
      visitedTableDateDbtypeCountMap.put(visitedTable, dateDbtypeCountMap);
    }
    dbtypeCountMap = dateDbtypeCountMap.get(visitedDate);
    if (null == dbtypeCountMap) {
      dbtypeCountMap = new ConcurrentHashMap<>();
      dateDbtypeCountMap.put(visitedDate, dbtypeCountMap);
    }
    // 只有当数据库操作成功的情况下，才更新本地内存里的数据。2022-06-16 15:05:56
    if (StringUtil.isNotBlank(dbType)) {
      dbtypeCountMap.put(dbType, visitedCount);
    }
  }

  /**
   * <B>方法名称：updateLocalMemoryNoEnableByUserPortraitByVisitedTable</B>
   * <B>概要说明：禁用这条规则；先把这个规则在内存中的统计次数保存到数据库中，然后在本地内存中先删除这条规则</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月16日 14:06:04
   * @Param [userPortraitByVisitedTableEverydayDo]
   **/
  private ServerResponse<String> updateLocalMemoryNoEnableByUserPortraitByVisitedTable(UserPortraitByVisitedTableEverydayDo userPortraitByVisitedTableEverydayDo) {
    Boolean userPortraitByVisitedTableEnable = AnomylyDetectionSingletonByVisitedTableEveryday.getUserPortraitByVisitedTableEnable();
    if(true == userPortraitByVisitedTableEnable){
      Map<String/* 用户名 */,
        Map<String/* 访问过的表 */,
          Map<String/* 访问日期，以天为单位 */,
            Map<String,/* 数据库操作类型：insert、delete、update、select */
              Integer/* 访问次数 */>>>> userPortraitByVisitedTableMap = AnomylyDetectionSingletonByVisitedTableEveryday.getUserPortraitByVisitedTableMap();
      String userName = userPortraitByVisitedTableEverydayDo.getUserName();
      String visitedTable = userPortraitByVisitedTableEverydayDo.getVisitedTable();
      String visitedDate = userPortraitByVisitedTableEverydayDo.getVisitedDate();
      String dbType = userPortraitByVisitedTableEverydayDo.getDbType();
      Integer visitedCount = userPortraitByVisitedTableEverydayDo.getVisitedCount();
      Map<String, Integer> dbtypeCountMap = null;
      Map<String, Map<String, Map<String, Integer>>> visitedTableDateDbtypeCountMap = userPortraitByVisitedTableMap.get(userName);
      if (null != userPortraitByVisitedTableMap) {
        if (null != visitedTableDateDbtypeCountMap) {
          Map<String, Map<String, Integer>> dateDbtypeCountMap = visitedTableDateDbtypeCountMap.get(visitedTable);
          if (null != dateDbtypeCountMap) {
            dbtypeCountMap = dateDbtypeCountMap.get(visitedDate);
            if (null != dbtypeCountMap) {
              Integer visitedCountFromLocalMemory = dbtypeCountMap.get(dbType);
              if (null != visitedCountFromLocalMemory && 0 <= visitedCountFromLocalMemory) {
                // 当要禁用这条规则时，要把这条规则在当前内存里的统计数据更新到数据库中；2022-06-16 15:07:11
                userPortraitByVisitedTableEverydayDo.setVisitedCount(visitedCount < visitedCountFromLocalMemory ? visitedCountFromLocalMemory : visitedCount);
              }
            }
          }
        }
      }
      int updateResult = userPortraitByVisitedTableEverydayMapper.updateByPrimaryKeySelective(userPortraitByVisitedTableEverydayDo);
      if (1 != updateResult) {
        log.error(" # UserPortraitByVisitedVisitedTableServiceImpl.updateLocalMemoryNoEnableByUserPortraitByVisitedTable() # 把禁用这条规则的状态更新到数据库中失败。");
        return ServerResponse.createByErrorMessage("更新数据库操作失败", "");
      }
      // 只有当数据库操作成功的情况下，才把本地内存里的数据删除。2022-06-16 15:05:56
      if (null != dbtypeCountMap) {
        dbtypeCountMap.remove(dbType);
      }
    }

    return ServerResponse.createBySuccess();
  }

  /**
   * <B>方法名称：getAllUserNameUserPortraitByVisitedTableEveryday</B>
   * <B>概要说明：获取所有的用户名</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月13日 10:06:00
   * @Param []
   **/
  @Override
  public ServerResponse<String> getAllUserNameUserPortraitByVisitedTableEveryday() {
    List<String> userNameList = userPortraitByVisitedTableEverydayMapper.selectAllUserName();
    ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    bySuccess.setData(JsonUtil.obj2String(userNameList));
    return bySuccess;
  }

  /**
   * <B>方法名称：statisticsEverydayVistedTableCount</B>
   * <B>概要说明：统计用户每天访问过的表的次数</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月08日 16:06:44
   * @Param [statisticsMap]
   **/
  private void statisticsEverydayVistedTableCount( Map<String/* 用户名 */,Map<String/* 数据库名 */,Map<String/* 访问过的表 */, Map<String/* 访问日期，以天为单位 */, Map<String,/* 数据库操作类型：insert、delete、update、select */ Integer/* 访问次数 */>>>>> userVisitedTableDateCountMap) {
    Instant selectNow = Instant.now();
    // 获取所有的历史数据，根据用户访问系统的时间，来计算其时间维度的画像。
    List<MsSegmentDetailDo> list = msSegmentDetailDao.selectAllUserNameIsNotNullAndTableNameIsNotNull();
    log.info("# UserPortraitByTimeServiceImpl.statisticsVistedTableCount() # 从数据库中查询出【{}】条用户访问过的表，用时 = 【{}】毫秒。", list.size(), DateTimeUtil.getTimeMillis(selectNow));
    if (null != list && 0 < list.size()) {
      for (MsSegmentDetailDo msSegmentDetailDo : list) {
        String dbType = msSegmentDetailDo.getDbType();
        String userName = msSegmentDetailDo.getUserName();
        String tableName = msSegmentDetailDo.getMsTableName();
        String startTime = msSegmentDetailDo.getStartTime();
        //lhx新增
        String dbInstance=msSegmentDetailDo.getDbInstance();

        Date date = DateTimeUtil.strToDate(startTime, DateTimeUtil.DATEFORMAT_STR_001);
        String strToDateToStr = DateTimeUtil.dateToStr(date, DateTimeUtil.DATEFORMAT_STR_002);


        msSegmentDetailDo.setUserPortraitFlagByVisitedTableEveryday(1);


        //lhx新增，再tablename外嵌套一层dbInstance
        Map<String/* 数据库名 */,Map<String/* 访问过的表 */, Map<String/* 访问日期，以天为单位 */, Map<String,/* 数据库操作类型：insert、delete、update、select */ Integer/* 访问次数 */>>>> visitedDbInstanceMap=userVisitedTableDateCountMap.get(userName);
        if (null == visitedDbInstanceMap) {
          visitedDbInstanceMap = new ConcurrentHashMap<>();
          userVisitedTableDateCountMap.put(userName,visitedDbInstanceMap);
        }


        Map<String/* 访问过的表 */, Map<String/* 访问日期，以天为单位 */, Map<String,/* 数据库操作类型：insert、delete、update、select */ Integer/* 访问次数 */>>> visitedTableDateCountMap = visitedDbInstanceMap.get(dbInstance);
        if (null == visitedTableDateCountMap) {
          visitedTableDateCountMap = new ConcurrentHashMap<>();
          visitedDbInstanceMap.put(dbInstance,visitedTableDateCountMap);
        }


        Map<String/* 访问日期，以天为单位 */, Map<String,/* 数据库操作类型：insert、delete、update、select */ Integer/* 访问次数 */>> dateDbTypeCountMap = visitedTableDateCountMap.get(tableName);
        if (null == dateDbTypeCountMap) {
          dateDbTypeCountMap = new ConcurrentHashMap<>();
          visitedTableDateCountMap.put(tableName, dateDbTypeCountMap);
        }

        Map<String,/* 数据库操作类型：insert、delete、update、select */ Integer/* 访问次数 */> dbTypeCountMap = dateDbTypeCountMap.get(strToDateToStr);
        if (null == dbTypeCountMap) {
          dbTypeCountMap = new ConcurrentHashMap<>();
          dateDbTypeCountMap.put(strToDateToStr, dbTypeCountMap);
        }
        Integer count = dbTypeCountMap.get(dbType);
        dbTypeCountMap.put(dbType, null == count ? 1 : count + 1);
      }
      try {
        // TODO：不应该放在这里更新，正确的做法是：当统计信息正常插入到数据库中之后，才能更新。否则，会造成这里已经更新成功，但统计信息插入到数据库失败的情况。2022-06-08 16:59:50
        Instant updateNow = Instant.now();
        msSegmentDetailDao.updateBatchById(list);
        log.info("# UserPortraitByTimeServiceImpl.statisticsVistedTableCount() # 批量更新基于访问过的表【{}条】标识耗时 = 【{}】毫秒。", list.size(), DateTimeUtil.getTimeMillis(updateNow));
      } catch (Exception e) {
        log.error("# UserPortraitByTimeServiceImpl.statisticsVistedTableCount() # 批量更新基于访问过的表标识出现了异常。", e);
      }
    }
  }

  /**
   * <B>方法名称：batchInsertUserPortraitByVisitedTable</B>
   * <B>概要说明：批量插入用户的访问过的表的统计数据</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月08日 16:06:35
   * @Param [list]
   **/
  private void batchInsertUserPortraitByVisitedTable(Map<String/* 用户名 */,Map<String/* 数据库名 */,Map<String/* 访问过的表 */, Map<String/* 访问日期，以天为单位 */, Map<String,/* 数据库操作类型：insert、delete、update、select */ Integer/* 访问次数 */>>>>> userVisitedTableDateCountMap) {
    try {
      List<UserPortraitByVisitedTableEverydayDo> list = new LinkedList<>();
      if (null == userVisitedTableDateCountMap || 0 == userVisitedTableDateCountMap.size()) {
        return;
      }
      Iterator<String> iterator0 = userVisitedTableDateCountMap.keySet().iterator();

      while (iterator0.hasNext()) {

        String userName = iterator0.next();
        Map<String/*数据库名*/,Map<String/* 访问过的表 */, Map<String/* 访问日期，以天为单位 */, Map<String,/* 数据库操作类型：insert、delete、update、select */ Integer/* 访问次数 */>>>>visitedDbInstanceDateCountMap = userVisitedTableDateCountMap.get(userName);
        Iterator<String> iterator1=visitedDbInstanceDateCountMap.keySet().iterator();
        while (iterator1.hasNext()){
          String dbInstance=iterator1.next();
          Map<String/* 访问过的表 */, Map<String/* 访问日期，以天为单位 */, Map<String,/* 数据库操作类型：insert、delete、update、select */ Integer/* 访问次数 */>>>visitedTableDateCountMap=visitedDbInstanceDateCountMap.get(dbInstance);
          Iterator<String> iterator2 = visitedTableDateCountMap.keySet().iterator();
          while (iterator2.hasNext()) {
            String tableName = iterator2.next();
            Map<String/* 访问日期，以天为单位 */, Map<String,/* 数据库操作类型：insert、delete、update、select */ Integer/* 访问次数 */>> dateDbTypeCountMap = visitedTableDateCountMap.get(tableName);
            Iterator<String> iterator3 = dateDbTypeCountMap.keySet().iterator();
            while (iterator3.hasNext()) {
              String dateTime = iterator3.next();
              Map<String,/* 数据库操作类型：insert、delete、update、select */ Integer/* 访问次数 */> dbTypeCountMap = dateDbTypeCountMap.get(dateTime);
              Iterator<String> iterator4 = dbTypeCountMap.keySet().iterator();
              while (iterator4.hasNext()) {
                UserPortraitByVisitedTableEverydayDo userPortraitByVisitedTableEverydayDo = new UserPortraitByVisitedTableEverydayDo();
                userPortraitByVisitedTableEverydayDo.setUserName(userName);
                String dbType = iterator4.next();
                Integer tableNameCount = dbTypeCountMap.get(dbType);
                userPortraitByVisitedTableEverydayDo.setDbType(dbType);
                userPortraitByVisitedTableEverydayDo.setVisitedTable(tableName);
                userPortraitByVisitedTableEverydayDo.setVisitedCount(tableNameCount);
                userPortraitByVisitedTableEverydayDo.setVisitedDate(dateTime);
                userPortraitByVisitedTableEverydayDo.setVisitedDbInstance(dbInstance);
                list.add(userPortraitByVisitedTableEverydayDo);
              }
            }
          }
        }
      }
      if (0 < list.size()) {
        Instant now = Instant.now();
        userPortraitByVisitedTableEverydayMapper.insertSelectiveBatch(list);
        log.info("# UserPortraitByTimeServiceImpl.batchInsertUserPortraitByVisitedTable() # 将统计好的用户访问次数【{}条】批量插入到数据库中用时 = 【{}】毫秒。", list.size(), DateTimeUtil.getTimeMillis(now));
      }
    } catch (Exception e) {
      log.error("# UserPortraitByTimeServiceImpl.batchInsertUserPortraitByVisitedTable() # 将统计好的用户访问次数批量插入到数据库中出现了异常。", e);
    }
  }

}
