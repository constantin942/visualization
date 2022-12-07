package com.aiit.skyflying.agent;

import com.aiit.skyflying.common.agent.AgentInformationSingleton;
import com.aiit.skyflying.common.constant.Const;
import com.aiit.skyflying.common.domain.MsAgentInformationDo;
import com.aiit.skyflying.common.utils.StringUtil;
import com.aiit.skyflying.common.dao.MsAgentInformationMapper;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.List;

/**
 * <B>主类名称: InitAgentInformationFromDb</B>
 * <B>概要说明：项目启动时，从数据库中加载探针信息</B>
 * @Author zm
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
    if (null != msAgentInformationDos && !msAgentInformationDos.isEmpty()) {
      for (MsAgentInformationDo msAgentInformationDo : msAgentInformationDos) {
        String agentCode = msAgentInformationDo.getAgentCode();
        String agentName = msAgentInformationDo.getAgentName();
        if (StringUtil.isBlank(agentName)) {
          agentName = Const.DOLLAR;
        }
        AgentInformationSingleton.put(agentCode, agentName);
      }
      AgentInformationSingleton.setAtomicBooleanToFalse();
    }
  }

}
