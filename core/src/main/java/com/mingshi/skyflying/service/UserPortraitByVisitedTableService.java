package com.mingshi.skyflying.service;

import com.mingshi.skyflying.domain.UserPortraitByVisitedTableDo;
import com.mingshi.skyflying.response.ServerResponse;

public interface UserPortraitByVisitedTableService extends ParentService<UserPortraitByVisitedTableDo, Long> {

  ServerResponse<String> createUserPortraitByVisitedTableEveryday();

}
