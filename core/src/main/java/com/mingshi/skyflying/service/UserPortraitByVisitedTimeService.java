package com.mingshi.skyflying.service;

import com.mingshi.skyflying.common.domain.UserPortraitByVisitedTimeDo;
import com.mingshi.skyflying.common.response.ServerResponse;


public interface UserPortraitByVisitedTimeService extends ParentService<UserPortraitByVisitedTimeDo, Long> {

    ServerResponse<String> createUserPortraitByVisitedTime();

		ServerResponse<String> getAllUserPortraitByVisitedTime(String userName, Integer pageNo, Integer pageSize);

    ServerResponse<String> getAllUserNamePortraitByVisitedTime();

		ServerResponse<String> updateUserPortraitByVisitedTimeRule(Integer ruleId, Integer isDelete);

    ServerResponse<String> addUserPortraitByVisitedTtimeRule(String userName, Integer forenoonCount, Integer afternoonCount, Integer nightCount);
}
