package com.mingshi.skyflying.impl;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.MsConfigDo;
import com.mingshi.skyflying.common.response.ServerResponse;
import com.mingshi.skyflying.common.utils.JsonUtil;
import com.mingshi.skyflying.common.utils.StringUtil;
import com.mingshi.skyflying.common.dao.MsConfigDao;
import com.mingshi.skyflying.service.MsConfigService;
import com.mingshi.skyflying.common.utils.MingshiServerUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;

/**
 * <B>方法名称：AuditLogServiceImpl</B>
 * <B>概要说明：将审计日志插入到数据库中</B>
 *
 * @Author zm
 * @Date 2022年05月25日 14:05:13
 * @Param
 * @return
 **/
@Slf4j
@Service("msConfigService")
public class MsConfigServiceImpl implements MsConfigService {

    @Resource
    private MsConfigDao msConfigDao;
    @Resource
    private MingshiServerUtil mingshiServerUtil;

    /**
     * <B>方法名称：getAkSkFromDb</B>
     * <B>概要说明：从MySQL中获取ak、sk</B>
     *
     * @return com.mingshi.skyflying.common.response.ServerResponse<java.lang.String>
     * @Author zm
     * @Date 2022-10-11 13:40:52
     * @Param []
     **/
    @Override
    public ServerResponse<String> getAkSkFromDb() {
        log.info("# MsConfigServiceImpl.getAkSkFromDb() # 开始获取ak、sk。");
        ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
        ObjectNode objectNode = mingshiServerUtil.getAkSk(Boolean.FALSE);
        if (null != objectNode) {
            bySuccess.setData(objectNode.toString());
        }
        log.info("# MsConfigServiceImpl.getAkSkFromDb() # 获取ak、sk完毕，返回给前端的数据 = 【{}】。", JsonUtil.obj2String(bySuccess));
        return bySuccess;
    }

    @Override
    public ServerResponse<String> setAkSkIntoDb(String ak, String sk) {
        try {
            return doSetAkSkIntoDb(ak, sk);
        } catch (Exception e) {
            log.error("# MsConfigServiceImpl.setAkSkIntoDb() # 开始保存或者更新ak、sk时，出现了异常。");
            return ServerResponse.createByError();
        }
    }

    @Override
    public ServerResponse<String> getRegionFromDb() {
        log.info("# MsConfigServiceImpl.getRegionFromDb() # 开始获取数据库所属区域信息。");
        ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
        MsConfigDo msConfigDo = msConfigDao.selectByConfigType(Const.DMS_REGION);
        if (null != msConfigDo) {
            bySuccess.setData(JsonUtil.obj2String(msConfigDo));
        }
        log.info("# MsConfigServiceImpl.getRegionFromDb() # 获取数据库所属区域信息完毕，返回给前端的数据 = 【{}】。", JsonUtil.obj2String(bySuccess));
        return bySuccess;
    }

    @Override
    public ServerResponse<String> setRegionIntoDb(String region) {
        try {
            return doSetRegionIntoDb(region);
        } catch (Exception e) {
            log.error("# MsConfigServiceImpl.setRegionIntoDb() # 开始保存或者更新数据库所属区域信息时，出现了异常。");
            return ServerResponse.createByError();
        }
    }

    /**
     * <B>方法名称：doSetRegionIntoDb</B>
     * <B>概要说明：保存或者更新数据库所属区域信息</B>
     *
     * @Author zm
     * @Date 2022-10-11 14:44:14
     * @Param [region]
     * @return com.mingshi.skyflying.common.response.ServerResponse<java.lang.String>
     **/
    private ServerResponse<String> doSetRegionIntoDb(String region) {
        log.info("# MsConfigServiceImpl.doSetRegionIntoDb() # 开始保存或者更新数据库所属区域信息。");

        ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
        if (StringUtil.isBlank(region)) {
            return ServerResponse.createByErrorMessage("数据库所属区域信息不能为空", "");
        }

        ObjectNode jsonObject = JsonUtil.createJsonObject();
        jsonObject.put(Const.DMS_REGION, region);

        MsConfigDo msConfigDo = msConfigDao.selectByConfigType(Const.DMS_REGION);
        if (null == msConfigDo || StringUtil.isBlank(msConfigDo.getConfig())) {
            // 不存在，则插入；
            msConfigDo = new MsConfigDo();
            msConfigDo.setConfigType(Const.DMS_REGION);
            msConfigDo.setConfig(jsonObject.toString());
            int result = msConfigDao.insertSelective(msConfigDo);
            if (!Const.NUMBER_ONE.equals(result)) {
                return ServerResponse.createByErrorMessage("数据库所属区域信息保存到数据库中失败", "");
            }
            log.info("# MsConfigServiceImpl.doSetRegionIntoDb() # 保存数据库所属区域信息成功。");
        } else {
            // 存在，则更新；
            msConfigDo.setConfig(jsonObject.toString());
            int updateResult = msConfigDao.updateByPrimaryKeySelective(msConfigDo);
            if (!Const.NUMBER_ONE.equals(updateResult)) {
                return ServerResponse.createByErrorMessage("数据库所属区域信息更新到数据库中失败", "");
            }
            log.info("# MsConfigServiceImpl.doSetRegionIntoDb() # 更新数据库所属区域信息成功。");
        }
        return bySuccess;
    }

    /**
     * <B>方法名称：doSetAkSkIntoDb</B>
     * <B>概要说明：开始保存或者更新ak、sk</B>
     *
     * @return com.mingshi.skyflying.common.response.ServerResponse<java.lang.String>
     * @Author zm
     * @Date 2022-10-11 14:37:09
     * @Param [ak, sk]
     **/
    private ServerResponse<String> doSetAkSkIntoDb(String ak, String sk) {
        log.info("# MsConfigServiceImpl.doSetAkSkIntoDb() # 开始保存或者更新ak、sk。");

        ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
        if (StringUtil.isBlank(ak)) {
            return ServerResponse.createByErrorMessage("ak不能为空", "");
        }
        if (StringUtil.isBlank(sk)) {
            return ServerResponse.createByErrorMessage("sk不能为空", "");
        }

        ObjectNode jsonObject = JsonUtil.createJsonObject();
        jsonObject.put(Const.AK, ak);
        jsonObject.put(Const.SK, sk);

        MsConfigDo msConfigDo = msConfigDao.selectByConfigType(Const.AK_SK);
        if (null == msConfigDo || StringUtil.isBlank(msConfigDo.getConfig())) {
            // 不存在，则插入；
            msConfigDo = new MsConfigDo();
            msConfigDo.setConfigType(Const.AK_SK);
            msConfigDo.setConfig(jsonObject.toString());
            int result = msConfigDao.insertSelective(msConfigDo);
            if (!Const.NUMBER_ONE.equals(result)) {
                return ServerResponse.createByErrorMessage("ak、sk保存到数据库中失败", "");
            }
            log.info("# MsConfigServiceImpl.doSetAkSkIntoDb() # 保存ak、sk成功。");
        } else {
            // 存在，则更新；
            msConfigDo.setConfig(jsonObject.toString());
            int updateResult = msConfigDao.updateByPrimaryKeySelective(msConfigDo);
            if (!Const.NUMBER_ONE.equals(updateResult)) {
                return ServerResponse.createByErrorMessage("ak、sk更新到数据库中失败", "");
            }
            log.info("# MsConfigServiceImpl.doSetAkSkIntoDb() # 更新ak、sk成功。");
        }
        return bySuccess;
    }
}
