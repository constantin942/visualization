package com.mingshi.skyflying.service;

import com.mingshi.skyflying.domain.UserPortraitByVisitedTimeDo;
import com.mingshi.skyflying.response.ServerResponse;

public interface UserPortraitByVisitedTimeService extends ParentService<UserPortraitByVisitedTimeDo, Long> {

    ServerResponse<String> createUserPortraitByVisitedTime();

		ServerResponse<String> getAllUserPortraitByVisitedTime(String userName, Integer pageNo, Integer pageSize);

    ServerResponse<String> getAllUserNamePortraitByVisitedTime();
}
