package com.mingshi.skyflying.config;

import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

import javax.annotation.PreDestroy;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * <B>主类名称: LinuxStateForShellConfig</B>
 * <B>概要说明：</B>
 * Author zm
 * Date 2022/8/4 15:49
 *
 * @Version 1.0
 **/
@Slf4j
@Component
public class LinuxStateForShellConfig implements ApplicationRunner {
  private static volatile Session session24;
  private static volatile Session session25;

  private static volatile AtomicBoolean atomicBoolean = new AtomicBoolean(false);

  @Override
  public void run(ApplicationArguments args) throws Exception {
    session24 = connect("", "", "");
    session25 = connect("", "", "");
    atomicBoolean.set(true);
  }

  public static Session getSession24() {
    return session24;
  }

  public static Session getSession25() {
    return session25;
  }

  public static boolean getAtomicBoolean() {
    return atomicBoolean.get();
  }

  /**
   * <B>方法名称：close</B>
   * <B>概要说明：项目关闭时，释放资源</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年08月04日 16:08:38
   * @Param []
   **/
  @PreDestroy
  private void close() {
    log.info("# LinuxStateForShellConfig.close() # 系统关闭，释放资源。");
    if (null != session24) {
      session24.disconnect();
    }
    if (null != session25) {
      session25.disconnect();
    }
  }

  /**
   * 连接到指定的HOST
   *
   * @return isConnect
   * @throws JSchException JSchException
   */
  private static Session connect(String user, String passwd, String host) {
    Session session = null;
    JSch jsch = new JSch();
    try {
      session = jsch.getSession(user, host, 22);
      session.setPassword(passwd);

      java.util.Properties config = new java.util.Properties();
      config.put("StrictHostKeyChecking", "no");
      session.setConfig(config);

      session.connect();
    } catch (JSchException e) {
      log.error("# LinuxStateForShellConfig.connect() # 根据用户名【{}】、密码、主机地址【{}】获取session时，出现了异常。", user, host, e);
      return null;
    }
    return session;
  }
}
