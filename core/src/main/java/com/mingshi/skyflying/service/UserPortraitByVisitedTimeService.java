package com.mingshi.skyflying.service;

import com.mingshi.skyflying.domain.UserPortraitByVisitedTimeDo;
import com.mingshi.skyflying.response.ServerResponse;

public interface UserPortraitByVisitedTimeService extends ParentService<UserPortraitByVisitedTimeDo, Long> {

  ServerResponse<String> createUserPortraitByVisitedTime();

}
