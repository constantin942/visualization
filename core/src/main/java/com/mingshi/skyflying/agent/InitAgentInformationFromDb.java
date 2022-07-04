package com.mingshi.skyflying.agent;

import com.mingshi.skyflying.constant.Const;
import com.mingshi.skyflying.dao.MsAgentInformationMapper;
import com.mingshi.skyflying.domain.MsAgentInformationDo;
import com.mingshi.skyflying.utils.StringUtil;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.List;

/**
 * <B>主类名称: InitAgentInformationFromDb</B>
 * <B>概要说明：项目启动时，从数据库中加载探针信息</B>
 * Author zm
 * Date 2022/6/28 16:51
 *
 * @Version 1.0
 **/
@Component
public class InitAgentInformationFromDb implements ApplicationRunner {

  @Resource
  private MsAgentInformationMapper msAgentInformationMapper;

  @Override
  public void run(ApplicationArguments args) throws Exception {
    setAgentInformation();
  }

  /**
   * <B>方法名称：setAgentInformation</B>
   * <B>概要说明：项目启动时，从数据库中加载探针的信息</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月28日 17:06:01
   * @Param []
   **/
  private void setAgentInformation() {
    List<MsAgentInformationDo> msAgentInformationDos = msAgentInformationMapper.selectAll();
    if (null != msAgentInformationDos && 0 < msAgentInformationDos.size()) {
      for (MsAgentInformationDo msAgentInformationDo : msAgentInformationDos) {
        String agentCode = msAgentInformationDo.getAgentCode();
        String agentName = msAgentInformationDo.getAgentName();
        if (StringUtil.isBlank(agentName)) {
          agentName = Const.DOLLAR;
        }
        AgentInformationSingleton.putIfAbsent(agentCode, agentName);
      }
      AgentInformationSingleton.setAtomicBooleanToFalse();
    }
  }

}