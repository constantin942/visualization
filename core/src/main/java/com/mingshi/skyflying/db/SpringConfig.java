package com.mingshi.skyflying.db;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DriverManagerDataSource;

/**
 * <B>方法名称：SpringConfig</B>
 * <B>概要说明：连接数据库的配置类，用于生成数据库设计文档</B>
 * @Author zm
 * @Date 2022年08月23日 14:08:45
 * @Param
 * @return
 **/
@Configuration
@ComponentScan(basePackages= {"com.mingshi.skyflying.db"})
@PropertySource("classpath:/db.properties")
public class SpringConfig {
	@Value("${db_url}")
	private String url;
	@Value("${db_username}")
	private String username;
	@Value("${db_password}")
	private String password;
	@Value("${db_driver}")
	private String driver;
	@Bean
	public DriverManagerDataSource driverManagerDataSource() {
		DriverManagerDataSource dataSource = new DriverManagerDataSource(url, username, password);
		dataSource.setDriverClassName(driver);
		return dataSource;
	}
	@Bean
	public JdbcTemplate jdbcTemplate() {
		return new JdbcTemplate(driverManagerDataSource());
	}
}
