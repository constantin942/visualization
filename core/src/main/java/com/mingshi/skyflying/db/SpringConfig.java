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
@PropertySource("classpath:application-${spring.profiles.active}.yml")
public class SpringConfig {
	@Value("${spring.datasource.url}")
	private String url;
	@Value("${spring.datasource.username}")
	private String username;
	@Value("${spring.datasource.password}")
	private String password;
	@Value("${spring.datasource.driver-class-name}")
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
