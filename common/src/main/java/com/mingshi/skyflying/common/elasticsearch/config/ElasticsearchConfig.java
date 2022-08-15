// package com.mingshi.skyflying.common.elasticsearch.config;
//
// import lombok.Data;
// import org.apache.http.HttpHost;
// import org.elasticsearch.client.RestClient;
// import org.elasticsearch.client.RestClientBuilder;
// import org.elasticsearch.client.RestHighLevelClient;
// import org.springframework.boot.context.properties.ConfigurationProperties;
// import org.springframework.context.annotation.Bean;
// import org.springframework.context.annotation.Configuration;
// import org.springframework.core.convert.support.DefaultConversionService;
// import org.springframework.data.elasticsearch.config.AbstractElasticsearchConfiguration;
// import org.springframework.data.elasticsearch.core.ElasticsearchEntityMapper;
// import org.springframework.data.elasticsearch.core.ElasticsearchRestTemplate;
// import org.springframework.data.elasticsearch.core.EntityMapper;
//
// @ConfigurationProperties(prefix = "elasticsearch")
// @Configuration
// @Data
// public class ElasticsearchConfig extends AbstractElasticsearchConfiguration {
//   private String host;
//   private Integer port;
//
//   @Override
//   public RestHighLevelClient elasticsearchClient() {
//     RestClientBuilder builder = RestClient.builder(new HttpHost(host, port));
//     RestHighLevelClient restHighLevelClient = new RestHighLevelClient(builder);
//     return restHighLevelClient;
//   }
//
//   @Bean
//   public ElasticsearchRestTemplate elasticsearchRestTemplate(RestHighLevelClient elasticsearchClient, EntityMapper entityMapper) {
//     return new ElasticsearchRestTemplate(elasticsearchClient, entityMapper);
//   }
//
//   /**
//    * 指定EntityMapper为ElasticsearchEntityMapper
//    * 解决es mapper映射实体类问题
//    *
//    * @return
//    */
//   @Bean
//   @Override
//   public EntityMapper entityMapper() {
//     ElasticsearchEntityMapper entityMapper = new ElasticsearchEntityMapper(elasticsearchMappingContext(), new DefaultConversionService());
//     entityMapper.setConversions(elasticsearchCustomConversions());
//     return entityMapper;
//   }
// }
