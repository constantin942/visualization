// package com.mingshi.skyflying.controller;
//
// import com.mingshi.skyflying.elasticsearch.domain.EsMsSegmentDetailDo;
// import com.mingshi.skyflying.elasticsearch.utils.MingshiElasticSearchUtil;
// import com.mingshi.skyflying.response.ServerResponse;
// import com.mingshi.skyflying.utils.JsonUtil;
// import lombok.extern.slf4j.Slf4j;
// import org.springframework.stereotype.Controller;
// import org.springframework.web.bind.annotation.RequestMapping;
// import org.springframework.web.bind.annotation.RequestMethod;
// import org.springframework.web.bind.annotation.RequestParam;
// import org.springframework.web.bind.annotation.ResponseBody;
//
// import javax.annotation.Resource;
//
// /**
//  * <B>主类名称: EsController</B>
//  * <B>概要说明：</B>
//  * Author zm
//  * Date 2022/7/2 19:02
//  *
//  * @Version 1.0
//  **/
// @Controller
// @Slf4j
// @RequestMapping("/es")
// public class EsController {
//   @Resource
//   private MingshiElasticSearchUtil mingshiElasticSearchUtil;
//
//   /**
//    * <B>方法名称：updateSkywalkingAgent</B>
//    * <B>概要说明：更新探针别名</B>
//    *
//    * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
//    * @Author zm
//    * @Date 2022年06月29日 14:06:30
//    * @Param [agentCode, pageNo, pageSize]
//    **/
//   @ResponseBody
//   @RequestMapping(value = "/filedIsNull", method = RequestMethod.GET)
//   public ServerResponse<String> filedIsNull(@RequestParam(value = "field") String field) {
//     Iterable<EsMsSegmentDetailDo> esMsSegmentDetailDos = mingshiElasticSearchUtil.termQueryByFiledsIsNull(field);
//     for (EsMsSegmentDetailDo esMsSegmentDetailDo : esMsSegmentDetailDos) {
//       log.info("" + JsonUtil.obj2String(esMsSegmentDetailDo));
//     }
//     return null;
//   }
// }
