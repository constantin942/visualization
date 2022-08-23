package com.mingshi.skyflying.common.elasticsearch.utils;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class MingshiElasticSearchUtil {
  //
  // @Autowired
  // private MsSegmentDetailEsDao msSegmentDetailEsDao;
  // @Qualifier
  // private ElasticsearchRestTemplate restTemplate;
  //
  // /**
  //  * 新增
  //  */
  // public void save(EsMsSegmentDetailDo esMsSegmentDetailDo) {
  //   msSegmentDetailEsDao.save(esMsSegmentDetailDo);
  // }
  //
  // // //POSTMAN, GET http://localhost:9200/esMsSegmentDetailDo/_doc/2
  // //
  // // //修改
  // // public void update() {
  // //   EsMsSegmentDetailDo esMsSegmentDetailDo = new EsMsSegmentDetailDo();
  // //   esMsSegmentDetailDo.setId(2L);
  // //   esMsSegmentDetailDo.setTitle("小米 2 手机");
  // //   esMsSegmentDetailDo.setCategory("手机");
  // //   esMsSegmentDetailDo.setPrice(9999.0);
  // //   esMsSegmentDetailDo.setImages("http://www.atguigu/xm.jpg");
  // //   msSegmentDetailEsDao.save(esMsSegmentDetailDo);
  // // }
  // // //POSTMAN, GET http://localhost:9200/esMsSegmentDetailDo/_doc/2
  //
  // public Iterable<EsMsSegmentDetailDo> findAll() {
  //   Iterable<EsMsSegmentDetailDo> esMsSegmentDetailDos = msSegmentDetailEsDao.findAll();
  //   return esMsSegmentDetailDos;
  // }
  //
  // //删除
  // public void delete(EsMsSegmentDetailDo esMsSegmentDetailDo) {
  //   msSegmentDetailEsDao.delete(esMsSegmentDetailDo);
  // }
  // //POSTMAN, GET http://localhost:9200/esMsSegmentDetailDo/_doc/2
  //
  // //批量新增
  // public void saveAll(List<EsMsSegmentDetailDo> esMsSegmentDetailDoList) {
  //   msSegmentDetailEsDao.saveAll(esMsSegmentDetailDoList);
  // }
  //
  // //分页查询
  // public Page<EsMsSegmentDetailDo> findByPageable(int currentPage/* 当前页，第一页从 0 开始， 1 表示第二页 */, int pageSize/* 每页显示多少条 */, String sortByField) {
  //   PageRequest pageRequest = null;
  //   if (StringUtil.isNotBlank(sortByField)) {
  //     //设置排序(排序方式，正序还是倒序，排序的 id)
  //     Sort sort = Sort.by(Sort.Direction.DESC, sortByField);
  //     //设置查询分页
  //     pageRequest = PageRequest.of(currentPage, pageSize, sort);
  //   } else {
  //     //设置查询分页
  //     pageRequest = PageRequest.of(currentPage, pageSize);
  //   }
  //
  //   //分页查询
  //   Page<EsMsSegmentDetailDo> esMsSegmentDetailDoPage = msSegmentDetailEsDao.findAll(pageRequest);
  //   return esMsSegmentDetailDoPage;
  // }
  //
  // /**
  //  * term 查询
  //  * search(termQueryBuilder) 调用搜索方法，参数查询构建器对象
  //  */
  // public Iterable<EsMsSegmentDetailDo> termQuery(String queryByField/* 根据那个字段查询 */, String fieldContent/* 查询字段的内容 */) {
  //   TermQueryBuilder termQueryBuilder = QueryBuilders.termQuery(queryByField, fieldContent);
  //   Iterable<EsMsSegmentDetailDo> esMsSegmentDetailDos = msSegmentDetailEsDao.search(termQueryBuilder);
  //   return esMsSegmentDetailDos;
  // }
  //
  // /**
  //  * <B>方法名称：termQueryByFiledsIsNull</B>
  //  * <B>概要说明：查询某个字段为空的文档/记录</B>
  //  *
  //  * @return java.lang.Iterable<com.mingshi.skyflying.common.elasticsearch.domain.EsMsSegmentDetailDo>
  //  * @Author zm
  //  * @Date 2022年07月02日 19:07:44
  //  * @Param [queryByField]
  //  * GET /segment_detail/_search
  //  * {
  //  *   "query": {
  //  *     "bool": {
  //  *       "should": [
  //  *         {"term": {"userName": ""}},
  //  *         {"bool": {"must_not":{"exists": {"field":"userName"}}}}
  //  *       ]
  //  *     }
  //  *   }
  //  * }
  //  * 上面es搜索语句对应的sql语句如下所示：
  //  * select *
  //  * from segment_detail
  //  * where userName is null or userName = ''
  //  **/
  // public Iterable<EsMsSegmentDetailDo> termQueryByFiledsIsNull(String queryByField/* 根据那个字段查询 */) {
  //   Iterable<EsMsSegmentDetailDo> esMsSegmentDetailDos = null;
  //   try {
  //     // 根据es的查询语句，先构造出来一个term查询；
  //     // 然后再构造一个bool、must_not、exists查询；
  //     // 最后将term查询和bool_must_not_exitst查询放入到一个should查询中；
  //     // 接着将should查询放入到一个bool查询中。
  //     TermQueryBuilder termQuery = QueryBuilders.termQuery(queryByField, "");
  //     BoolQueryBuilder boolMustNotExistsQueryBuilder = QueryBuilders.boolQuery().mustNot(QueryBuilders.existsQuery(queryByField));
  //     BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery().must(QueryBuilders.boolQuery().should(termQuery).should(boolMustNotExistsQueryBuilder));
  //     esMsSegmentDetailDos = msSegmentDetailEsDao.search(boolQueryBuilder);
  //   } catch (Exception e) {
  //     log.error("# MingshiElasticSearchUtil.termQueryByFiledsIsNull() # 查询字段【{}】为空的记录时，出现了异常。", queryByField);
  //   }
  //   return esMsSegmentDetailDos;
  // }
  //
  // /** 使用term filter/query查询时，所要查询的字段不分词，直接去倒排索引中原样查找全字段匹配。比如有一个字段是user_name，我们要找user_name中是zhangsan的这个人的信息。
  //  *  当我们输入zhangsan时，直接去倒排索引中匹配zhangsan。
  //  * term 查询加分页
  //  */
  // public Iterable<EsMsSegmentDetailDo> termQueryByPage(int currentPage, int pageSize, String queryByField/* 根据那个字段查询 */, String fieldContent/* 查询字段的内容 */, String sortByField/* 排序字段 */) throws Exception {
  //   if (currentPage < 0) {
  //     throw new Exception("当前页码不能小于0");
  //   }
  //
  //   if (pageSize < 0) {
  //     throw new Exception("每页记录大小不能小于0");
  //   }
  //
  //   if (0 == pageSize) {
  //     throw new Exception("每页记录大小不能等于0");
  //   }
  //
  //   //设置查询分页
  //   PageRequest pageRequest = null;
  //   if (StringUtil.isNotBlank(sortByField)) {
  //     //设置排序(排序方式，正序还是倒序，排序的 id)
  //     Sort sort = Sort.by(Sort.Direction.DESC, sortByField);
  //     //设置查询分页
  //     pageRequest = PageRequest.of(currentPage, pageSize, sort);
  //   } else {
  //     //设置查询分页
  //     pageRequest = PageRequest.of(currentPage, pageSize);
  //   }
  //
  //   TermQueryBuilder termQueryBuilder = QueryBuilders.termQuery(queryByField, fieldContent);
  //   Iterable<EsMsSegmentDetailDo> esMsSegmentDetailDos = msSegmentDetailEsDao.search(termQueryBuilder, pageRequest);
  //   return esMsSegmentDetailDos;
  // }
  // public List<EsMsSegmentDetailDo> termQueryByMap(Map<String, Object> map) throws Exception {
  //   List<EsMsSegmentDetailDo> list = new LinkedList<>();
  //   String queryByField/* 根据那个字段查询 */;
  //   String fieldContent/* 查询字段的内容 */;
  //   String sortByField = Const.START_TIME;/* 排序字段 */;
  //
  //   NativeSearchQueryBuilder nativeSearchQueryBuilder =  new NativeSearchQueryBuilder();
  //
  //   // 2. 构建bool查询；
  //   BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery();
  //   String userName = String.valueOf(map.get("userName"));
  //   if(StringUtil.isNotBlank(userName)){
  //     boolQueryBuilder.must(QueryBuilders.termsQuery("userName", "admin"));
  //   }
  //   String dbType = String.valueOf(map.get("dbType"));
  //   if(StringUtil.isNotBlank(dbType)){
  //     boolQueryBuilder.must(QueryBuilders.termsQuery(dbType, dbType));
  //   }
  //   String msTableName = String.valueOf(map.get("msTableName"));
  //   if(StringUtil.isNotBlank(msTableName)){
  //     boolQueryBuilder.must(QueryBuilders.termsQuery("msTableName", msTableName));
  //   }
  //   String dbUserName = String.valueOf(map.get("dbUserName"));
  //   if(StringUtil.isNotBlank(dbUserName)){
  //     boolQueryBuilder.must(QueryBuilders.termsQuery("dbUserName", dbUserName));
  //   }
  //
  //   // 分组查询
  //   TermsAggregationBuilder termsAgg = AggregationBuilders.terms("groupByGlobalTraceId").field("globalTraceId");
  //
  //   String startTime = String.valueOf(map.get(Const.START_TIME));
  //   String endTime = String.valueOf(map.get(Const.END_TIME));
  //
  //   nativeSearchQueryBuilder.withFilter(boolQueryBuilder);
  //   nativeSearchQueryBuilder.addAggregation(termsAgg);
  //
  //
  //   String aggName = "status_bucket";
  //
  //   nativeSearchQueryBuilder.withPageable(PageRequest.of(0,1));
  //
  //   nativeSearchQueryBuilder.addAggregation(termsAgg);
  //   NativeSearchQuery build = nativeSearchQueryBuilder.build();
  //
  //   Aggregations aggregations = restTemplate.query(build, SearchResponse::getAggregations);
  //   Terms terms = aggregations.get(aggName);
  //   List<? extends Terms.Bucket> buckets = terms.getBuckets();
  //   HashMap<String,Long> statusRes = new HashMap<>(Const.NUMBER_EIGHT);
  //   buckets.forEach(bucket -> {
  //     statusRes.put(bucket.getKeyAsString(),bucket.getDocCount());
  //   });
  //
  //
  //   Integer pageNo = Integer.parseInt((String) map.get(Const.PAGE_NO));
  //   Integer pageSize = Integer.parseInt((String) map.get("pageSize"));
  //
  //   //设置查询分页
  //   PageRequest pageRequest = null;
  //   if (StringUtil.isNotBlank(sortByField)) {
  //     //设置排序(排序方式，正序还是倒序，排序的 id)
  //     Sort sort = Sort.by(Sort.Direction.DESC, sortByField);
  //     //设置查询分页
  //     pageRequest = PageRequest.of(pageNo, pageSize, sort);
  //   } else {
  //     //设置查询分页
  //     pageRequest = PageRequest.of(pageNo, pageSize);
  //   }
  //
  //   Page<EsMsSegmentDetailDo> esMsSegmentDetailDos = msSegmentDetailEsDao.search(boolQueryBuilder, pageRequest);
  //   List<EsMsSegmentDetailDo> content = esMsSegmentDetailDos.getContent();
  //   return content;
  // }
}