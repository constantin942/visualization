package com.mingshi.skyflying.elasticsearch.dao;

import com.mingshi.skyflying.elasticsearch.domain.EsMsSegmentDetailDo;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface MsSegmentDetailEsDao extends ElasticsearchRepository<EsMsSegmentDetailDo, Long>{

}
