/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package com.mingshi.skyflying.component;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The supported list of skywalking java sniffer.
 */
public class ComponentsDefine {

  private final static Map<Integer, String> componentMap = new ConcurrentHashMap<>();

  public static Map<Integer, String> getComponentMap() {
    return componentMap;
  }

  static {
    componentMap.put(1, "Tomcat");
    componentMap.put(2, "HttpClient");
    componentMap.put(3, "Dubbo");
    componentMap.put(8, "Motan");
    componentMap.put(10, "Resin");
    componentMap.put(11, "Feign");
    componentMap.put(12, "OKHttp");
    componentMap.put(13, "SpringRestTemplate");
    componentMap.put(14, "SpringMVC");
    componentMap.put(15, "Struts2");
    componentMap.put(16, "NutzMVC");
    componentMap.put(17, "NutzHttp");
    componentMap.put(18, "JettyClient");
    componentMap.put(19, "JettyServer");
    componentMap.put(21, "ShardingJDBC");
    componentMap.put(23, "GRPC");
    componentMap.put(24, "ElasticJob");
    componentMap.put(26, "httpasyncclient");
    componentMap.put(28, "ServiceComb");
    componentMap.put(29, "Hystrix");
    componentMap.put(30, "Jedis");
    componentMap.put(32, "h2-jdbc-driver");
    componentMap.put(33, "mysql-connector-java");
    componentMap.put(34, "ojdbc");
    componentMap.put(35, "Spymemcached");
    componentMap.put(36, "Xmemcached");
    componentMap.put(37, "postgresql-jdbc-driver");
    componentMap.put(38, "rocketMQ-producer");
    componentMap.put(39, "rocketMQ-consumer");
    componentMap.put(40, "kafka-producer");
    componentMap.put(41, "kafka-consumer");
    componentMap.put(42, "mongodb-driver");
    componentMap.put(43, "SOFARPC");
    componentMap.put(45, "activemq-producer");
    componentMap.put(46, "activemq-consumer");
    componentMap.put(48, "transport-client");
    componentMap.put(52, "rabbitmq-producer");
    componentMap.put(53, "rabbitmq-consumer");
    componentMap.put(54, "Canal");
    componentMap.put(55, "Gson");
    componentMap.put(56, "Redisson");
    componentMap.put(57, "Lettuce");
    componentMap.put(58, "Zookeeper");
    componentMap.put(59, "Vert.x");
    componentMap.put(60, "ShardingSphere");
    componentMap.put(61, "spring-cloud-gateway");
    componentMap.put(62, "RESTEasy");
    componentMap.put(63, "solrj");
    componentMap.put(65, "SpringAsync");
    componentMap.put(66, "JdkHttp");
    componentMap.put(67, "spring-webflux");
    componentMap.put(68, "Play");
    componentMap.put(69, "cassandra-java-driver");
    componentMap.put(71, "Light4J");
    componentMap.put(73, "pulsar-producer");
    componentMap.put(74, "pulsar-consumer");
    componentMap.put(75, "Ehcache");
    componentMap.put(76, "SocketIO");
    componentMap.put(77, "rest-high-level-client");
    componentMap.put(78, "spring-tx");
    componentMap.put(79, "Armeria");
    componentMap.put(80, "JdkThreading");
    componentMap.put(81, "KotlinCoroutine");
    componentMap.put(82, "AvroServer");
    componentMap.put(83, "AvroClient");
    componentMap.put(84, "Undertow");
    componentMap.put(85, "Finagle");
    componentMap.put(87, "mariadb-jdbc");
    componentMap.put(88, "quasar");
    componentMap.put(90, "influxdb-java");
    componentMap.put(91, "brpc-java");
    componentMap.put(92, "GraphQL");
    componentMap.put(93, "spring-annotation");
    componentMap.put(94, "HBase");
    componentMap.put(95, "spring-kafka-consumer");
    componentMap.put(96, "SpringScheduled");
    componentMap.put(97, "quartz-scheduler");
    componentMap.put(98, "xxl-job");
    componentMap.put(99, "spring-webflux-webclient");
    componentMap.put(100, "thrift-server");
    componentMap.put(101, "thrift-client");
    componentMap.put(102, "AsyncHttpClient");
    componentMap.put(103, "dbcp");
    componentMap.put(104, "mssql-jdbc-driver");
    componentMap.put(105, "Apache-CXF");
    componentMap.put(106, "dolphinscheduler");
    componentMap.put(107, "JsonRpc");
    componentMap.put(108, "Seata");
    componentMap.put(109, "MyBatis");
    componentMap.put(112, "Neo4j");
    componentMap.put(113, "Sentinel");
    componentMap.put(114, "GuavaCache");

    componentMap.put(115, "AlibabaDruid");
    componentMap.put(116, "HikariCP");
    componentMap.put(117, "Fastjson");
    componentMap.put(118, "Jackson");
    componentMap.put(119, "ClickHouse-jdbc-driver");
    componentMap.put(121, "apache-kylin-jdbc-driver");
    componentMap.put(123, "GuavaEventBus");
    componentMap.put(127, "Apache-ShenYu");
    componentMap.put(128, "send-email");
    componentMap.put(129, "file-output");

  }

  public static final OfficialComponent TOMCAT = new OfficialComponent(1, "Tomcat");
  public static final OfficialComponent HTTPCLIENT = new OfficialComponent(2, "HttpClient");
  public static final OfficialComponent DUBBO = new OfficialComponent(3, "Dubbo");
  public static final OfficialComponent MOTAN = new OfficialComponent(8, "Motan");
  public static final OfficialComponent RESIN = new OfficialComponent(10, "Resin");
  public static final OfficialComponent FEIGN = new OfficialComponent(11, "Feign");
  public static final OfficialComponent OKHTTP = new OfficialComponent(12, "OKHttp");
  public static final OfficialComponent SPRING_REST_TEMPLATE = new OfficialComponent(13, "SpringRestTemplate");
  public static final OfficialComponent SPRING_MVC_ANNOTATION = new OfficialComponent(14, "SpringMVC");
  public static final OfficialComponent STRUTS2 = new OfficialComponent(15, "Struts2");
  public static final OfficialComponent NUTZ_MVC_ANNOTATION = new OfficialComponent(16, "NutzMVC");
  public static final OfficialComponent NUTZ_HTTP = new OfficialComponent(17, "NutzHttp");
  public static final OfficialComponent JETTY_CLIENT = new OfficialComponent(18, "JettyClient");
  public static final OfficialComponent JETTY_SERVER = new OfficialComponent(19, "JettyServer");
  public static final OfficialComponent SHARDING_JDBC = new OfficialComponent(21, "ShardingJDBC");
  public static final OfficialComponent GRPC = new OfficialComponent(23, "GRPC");
  public static final OfficialComponent ELASTIC_JOB = new OfficialComponent(24, "ElasticJob");
  public static final OfficialComponent HTTP_ASYNC_CLIENT = new OfficialComponent(26, "httpasyncclient");
  public static final OfficialComponent SERVICECOMB = new OfficialComponent(28, "ServiceComb");
  public static final OfficialComponent HYSTRIX = new OfficialComponent(29, "Hystrix");
  public static final OfficialComponent JEDIS = new OfficialComponent(30, "Jedis");
  public static final OfficialComponent H2_JDBC_DRIVER = new OfficialComponent(32, "h2-jdbc-driver");
  public static final OfficialComponent MYSQL_JDBC_DRIVER = new OfficialComponent(33, "mysql-connector-java");
  public static final OfficialComponent OJDBC = new OfficialComponent(34, "ojdbc");
  public static final OfficialComponent SPYMEMCACHED = new OfficialComponent(35, "Spymemcached");
  public static final OfficialComponent XMEMCACHED = new OfficialComponent(36, "Xmemcached");
  public static final OfficialComponent POSTGRESQL_DRIVER = new OfficialComponent(37, "postgresql-jdbc-driver");
  public static final OfficialComponent ROCKET_MQ_PRODUCER = new OfficialComponent(38, "rocketMQ-producer");
  public static final OfficialComponent ROCKET_MQ_CONSUMER = new OfficialComponent(39, "rocketMQ-consumer");
  public static final OfficialComponent KAFKA_PRODUCER = new OfficialComponent(40, "kafka-producer");
  public static final OfficialComponent KAFKA_CONSUMER = new OfficialComponent(41, "kafka-consumer");
  public static final OfficialComponent MONGO_DRIVER = new OfficialComponent(42, "mongodb-driver");
  public static final OfficialComponent SOFARPC = new OfficialComponent(43, "SOFARPC");
  public static final OfficialComponent ACTIVEMQ_PRODUCER = new OfficialComponent(45, "activemq-producer");
  public static final OfficialComponent ACTIVEMQ_CONSUMER = new OfficialComponent(46, "activemq-consumer");
  public static final OfficialComponent TRANSPORT_CLIENT = new OfficialComponent(48, "transport-client");
  public static final OfficialComponent RABBITMQ_PRODUCER = new OfficialComponent(52, "rabbitmq-producer");
  public static final OfficialComponent RABBITMQ_CONSUMER = new OfficialComponent(53, "rabbitmq-consumer");
  public static final OfficialComponent CANAL = new OfficialComponent(54, "Canal");
  public static final OfficialComponent GSON = new OfficialComponent(55, "Gson");
  public static final OfficialComponent REDISSON = new OfficialComponent(56, "Redisson");
  public static final OfficialComponent LETTUCE = new OfficialComponent(57, "Lettuce");
  public static final OfficialComponent ZOOKEEPER = new OfficialComponent(58, "Zookeeper");
  public static final OfficialComponent VERTX = new OfficialComponent(59, "Vert.x");
  public static final OfficialComponent SHARDING_SPHERE = new OfficialComponent(60, "ShardingSphere");
  public static final OfficialComponent SPRING_CLOUD_GATEWAY = new OfficialComponent(61, "spring-cloud-gateway");
  public static final OfficialComponent RESTEASY = new OfficialComponent(62, "RESTEasy");
  public static final OfficialComponent SOLRJ = new OfficialComponent(63, "solrj");
  public static final OfficialComponent SPRING_ASYNC = new OfficialComponent(65, "SpringAsync");
  public static final OfficialComponent JDK_HTTP = new OfficialComponent(66, "JdkHttp");
  public static final OfficialComponent SPRING_WEBFLUX = new OfficialComponent(67, "spring-webflux");
  public static final OfficialComponent PLAY = new OfficialComponent(68, "Play");
  public static final OfficialComponent CASSANDRA_JAVA_DRIVER = new OfficialComponent(69, "cassandra-java-driver");
  public static final OfficialComponent LIGHT_4J = new OfficialComponent(71, "Light4J");
  public static final OfficialComponent PULSAR_PRODUCER = new OfficialComponent(73, "pulsar-producer");
  public static final OfficialComponent PULSAR_CONSUMER = new OfficialComponent(74, "pulsar-consumer");
  public static final OfficialComponent EHCACHE = new OfficialComponent(75, "Ehcache");
  public static final OfficialComponent SOCKET_IO = new OfficialComponent(76, "SocketIO");
  public static final OfficialComponent REST_HIGH_LEVEL_CLIENT = new OfficialComponent(77, "rest-high-level-client");
  public static final OfficialComponent SPRING_TX = new OfficialComponent(78, "spring-tx");
  public static final OfficialComponent ARMERIA = new OfficialComponent(79, "Armeria");
  public static final OfficialComponent JDK_THREADING = new OfficialComponent(80, "JdkThreading");
  public static final OfficialComponent KT_COROUTINE = new OfficialComponent(81, "KotlinCoroutine");
  public static final OfficialComponent AVRO_SERVER = new OfficialComponent(82, "AvroServer");
  public static final OfficialComponent AVRO_CLIENT = new OfficialComponent(83, "AvroClient");
  public static final OfficialComponent UNDERTOW = new OfficialComponent(84, "Undertow");
  public static final OfficialComponent FINAGLE = new OfficialComponent(85, "Finagle");
  public static final OfficialComponent MARIADB_JDBC = new OfficialComponent(87, "mariadb-jdbc");
  public static final OfficialComponent QUASAR = new OfficialComponent(88, "quasar");
  public static final OfficialComponent INFLUXDB_JAVA = new OfficialComponent(90, "influxdb-java");
  public static final OfficialComponent BRPC_JAVA = new OfficialComponent(91, "brpc-java");
  public static final OfficialComponent GRAPHQL = new OfficialComponent(92, "GraphQL");
  public static final OfficialComponent SPRING_ANNOTATION = new OfficialComponent(93, "spring-annotation");
  public static final OfficialComponent HBASE = new OfficialComponent(94, "HBase");
  public static final OfficialComponent SPRING_KAFKA_CONSUMER = new OfficialComponent(95, "spring-kafka-consumer");
  public static final OfficialComponent SPRING_SCHEDULED = new OfficialComponent(96, "SpringScheduled");
  public static final OfficialComponent QUARTZ_SCHEDULER = new OfficialComponent(97, "quartz-scheduler");
  public static final OfficialComponent XXL_JOB = new OfficialComponent(98, "xxl-job");
  public static final OfficialComponent SPRING_WEBCLIENT = new OfficialComponent(99, "spring-webflux-webclient");
  public static final OfficialComponent THRIFT_SERVER = new OfficialComponent(100, "thrift-server");
  public static final OfficialComponent THRIFT_CLIENT = new OfficialComponent(101, "thrift-client");
  public static final OfficialComponent ASYNC_HTTP_CLIENT = new OfficialComponent(102, "AsyncHttpClient");
  public static final OfficialComponent DBCP = new OfficialComponent(103, "dbcp");
  public static final OfficialComponent MSSQL_JDBC_DRIVER = new OfficialComponent(104, "mssql-jdbc-driver");
  public static final OfficialComponent APACHE_CXF = new OfficialComponent(105, "Apache-CXF");
  public static final OfficialComponent DOLPHIN_SCHEDULER = new OfficialComponent(106, "dolphinscheduler");
  public static final OfficialComponent JSON_RPC = new OfficialComponent(107, "JsonRpc");
  public static final OfficialComponent SEATA = new OfficialComponent(108, "Seata");
  public static final OfficialComponent MYBATIS = new OfficialComponent(109, "MyBatis");
  public static final OfficialComponent NEO4J = new OfficialComponent(112, "Neo4j");
  public static final OfficialComponent SENTINEL = new OfficialComponent(113, "Sentinel");
  public static final OfficialComponent GUAVA_CACHE = new OfficialComponent(114, "GuavaCache");

  public static final OfficialComponent ALIBABA_DRUID = new OfficialComponent(115, "AlibabaDruid");
  public static final OfficialComponent HIKARI_CP = new OfficialComponent(116, "HikariCP");
  public static final OfficialComponent FASTJSON = new OfficialComponent(117, "Fastjson");
  public static final OfficialComponent JACKSON = new OfficialComponent(118, "Jackson");
  public static final OfficialComponent CLICKHOUSE_JDBC_DRIVER = new OfficialComponent(119, "ClickHouse-jdbc-driver");
  public static final OfficialComponent APACHE_KYLIN_JDBC_DRIVER = new OfficialComponent(121, "apache-kylin-jdbc-driver");
  public static final OfficialComponent GUAVA_EVENT_BUS = new OfficialComponent(123, "GuavaEventBus");
  public static final OfficialComponent APACHE_SHENYU = new OfficialComponent(127, "Apache-ShenYu");
  public static final OfficialComponent SEND_EMAIL = new OfficialComponent(128, "send-email");
  public static final OfficialComponent FILE_OUTPUT = new OfficialComponent(129, "file-output");

}
