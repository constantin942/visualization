package com.mingshi.skyflying.service;

import com.mingshi.skyflying.domain.UserPortraitByVisitedTableDo;
import com.mingshi.skyflying.response.ServerResponse;

public interface UserPortraitByVisitedTableService extends ParentService<UserPortraitByVisitedTableDo, Long> {

  ServerResponse<String> createUserPortraitByVisitedTableEveryday();

  ServerResponse<String> getUserPortraitByVisitedTableEveryday(String userName, String visitedTable, String visitedDate, String visitedDbInstance,Integer pageNo, Integer pageSize);

  ServerResponse<String> getAllUserNameUserPortraitByVisitedTableEveryday();

  ServerResponse<String> getAllVisitedTablePortraitByVisitedTableEveryday();

  ServerResponse<String> updateUserPortraitByVisitedTableEverydayRule(Integer ruleId, Integer isDelete);

  ServerResponse<String> addUserPortraitByVisitedTableEverydayRule(String userName, String visitedTable, Integer visitedCount, String visitedDate, String dbType);

}
