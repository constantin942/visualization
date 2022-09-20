package com.mingshi.skyflying.common.utils;

import com.jcraft.jsch.Channel;
import com.jcraft.jsch.ChannelExec;
import com.jcraft.jsch.Session;
import com.mingshi.skyflying.common.constant.Const;
import lombok.extern.slf4j.Slf4j;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.text.DecimalFormat;
import java.util.HashMap;
import java.util.Map;

@Slf4j
public class LinuxStateForShellUtil {

  public static final String CPU_MEM_SHELL = "top -b -n 1";
  public static final String FILES_SHELL = "df -hl";
  public static final String[] COMMANDS = {CPU_MEM_SHELL};
  public static final String LINE_SEPARATOR = System.getProperty("line.separator");

  /**
   * 远程连接Linux 服务器 执行相关的命令
   *
   * @return 最终命令返回信息
   */
  public static Map<String, String> runDistanceShell(Session session) {
    Map<String, String> map = new HashMap<>(Const.NUMBER_EIGHT);
    StringBuilder stringBuffer;

    BufferedReader reader = null;
    Channel channel = null;
    try {
      for (String command : COMMANDS) {
        stringBuffer = new StringBuilder();
        channel = session.openChannel("exec");
        ((ChannelExec) channel).setCommand(command);

        channel.setInputStream(null);
        ((ChannelExec) channel).setErrStream(System.err);

        channel.connect();
        InputStream in = channel.getInputStream();
        reader = new BufferedReader(new InputStreamReader(in));
        String buf;
        while ((buf = reader.readLine()) != null) {

          //舍弃PID 进程信息
          if (buf.contains("PID")) {
            break;
          }
          stringBuffer.append(buf.trim()).append(LINE_SEPARATOR);
        }
        //每个命令存储自己返回数据-用于后续对返回数据进行处理
        map.put(command, stringBuffer.toString());
      }
    } catch (Exception e) {
      log.error("# LinuxStateForShellUtil.runDistanceShell() # 远程连接Linux服务器，然后执行相关的命令，出现了异常。", e);
    } finally {
      try {
        if (reader != null) {
          reader.close();
        }
      } catch (IOException e) {
        // ignore
      }
      if (channel != null) {
        channel.disconnect();
      }
    }
    return map;
  }


  /**
   * 直接在本地执行 shell
   *
   * @param commands 执行的脚本
   * @return 执行结果信息
   */
  public static Map<String, String> runLocalShell(String[] commands) {
    Runtime runtime = Runtime.getRuntime();

    Map<String, String> map = new HashMap<>(Const.NUMBER_EIGHT);
    StringBuilder stringBuffer;

    BufferedReader reader;
    Process process;
    for (String command : commands) {
      stringBuffer = new StringBuilder();
      try {
        process = runtime.exec(command);
        InputStream inputStream = process.getInputStream();
        reader = new BufferedReader(new InputStreamReader(inputStream));
        String buf;
        while ((buf = reader.readLine()) != null) {
          //舍弃PID 进程信息
          if (buf.contains("PID")) {
            break;
          }
          stringBuffer.append(buf.trim()).append(LINE_SEPARATOR);
        }

      } catch (IOException e) {
        log.error("# LinuxStateForShellUtil.runLocalShell() # 直接在本地执行 shell 时，出现了异常。", e);
        return null;
      }
      //每个命令存储自己返回数据-用于后续对返回数据进行处理
      map.put(command, stringBuffer.toString());
    }
    return map;
  }

  /**
   * 处理 shell 返回的信息
   * <p>
   * 具体处理过程以服务器返回数据格式为准
   * 不同的Linux 版本返回信息格式不同
   *
   * @param result shell 返回的信息
   * @return 最终处理后的信息
   */
  public static String disposeResultMessage(Map<String, String> result) {

    StringBuilder buffer = new StringBuilder();

    for (String command : COMMANDS) {
      String commandResult = result.get(command);
      if (null == commandResult) {
        continue;
      }

      if (command.equals(CPU_MEM_SHELL)) {
        String[] strings = commandResult.split(LINE_SEPARATOR);
        //将返回结果按换行符分割
        for (String line : strings) {
          // line = line.toUpperCase();//转大写处理

          //处理CPU Cpu(s): 10.8%us,  0.9%sy,  0.0%ni, 87.6%id,  0.7%wa,  0.0%hi,  0.0%si,  0.0%st
          if (line.startsWith("%Cpu(s):")) {

            try {
              String us = line.split(":")[1].split(Const.EN_COMMA)[0].replace("us", "").trim();
              if(!"0.0".equals(us)){
                String cpuStr = "CPU用户使用占有率:";
                cpuStr += us;
                buffer.append(cpuStr).append(LINE_SEPARATOR);
              }
            } catch (Exception e) {
              log.error("# LinuxStateForShellUtil.disposeResultMessage() # 获取cpu用户使用占有率时，出现了异常。", e);
            }
            //处理内存 Mem:  66100704k total, 65323404k used,   777300k free,    89940k buffers
          } else if (line.startsWith("KiB Mem")) {
            String memStr = "内存已使用:";
            // String memStr = "内存使用情况:";
            try {
              // memStr += line.split(":")[1]
              //   .replace("total", "总计")
              //   .replace("used", "已使用")
              //   .replace("free", "空闲")
              //   .replace("buff/cache", "缓存");
              String trim = line.split(":")[1].replace("total", "").replace("free", "").replace("used", "").replace("buff/cache", "").split(Const.EN_COMMA)[2].trim();
              double v = Double.parseDouble(trim) / 1024 / 1024;
              DecimalFormat decimalFormat = new DecimalFormat("######0.00");
              memStr += decimalFormat.format(v) + " GB";
            } catch (Exception e) {
              log.error("# LinuxStateForShellUtil.disposeResultMessage() # 获取内存使用率时，出现了异常。", e);
              continue;
            }
            int length = buffer.length();
            if(0 < length){
              buffer.append(memStr).append(LINE_SEPARATOR);
            }
          }
        }
      } else if (command.equals(FILES_SHELL)) {
        //处理系统磁盘状态
        // buffer.append("系统磁盘状态:");
        // try {
        //   buffer.append(disposeFilesSystem(commandResult)).append(LINE_SEPARATOR);
        // } catch (Exception e) {
        //   e.printStackTrace();
        //   buffer.append("计算过程出错").append(LINE_SEPARATOR);
        // log.error("# LinuxStateForShellUtil.disposeResultMessage() # 处理系统磁盘状态时，出现了异常。", e);
        // }
      }
    }

    return buffer.toString();
  }

  //处理系统磁盘状态

  /**
   * Filesystem            Size  Used Avail Use% Mounted on
   * /dev/sda3             442G  327G   93G  78% /
   * tmpfs                  32G     0   32G   0% /dev/shm
   * /dev/sda1             788M   60M  689M   8% /boot
   * /dev/md0              1.9T  483G  1.4T  26% /ezsonar
   *
   * @param commandResult 处理系统磁盘状态shell执行结果
   * @return 处理后的结果
   */
  private static String disposeFilesSystem(String commandResult) {
    String[] strings = commandResult.split(LINE_SEPARATOR);

    // final String PATTERN_TEMPLATE = "([a-zA-Z0-9%_/]*)\\s";
    int size = 0;
    int used = 0;
    for (int i = 0; i < strings.length - 1; i++) {
      if (i == 0) {
        continue;
      }

      int temp = 0;
      for (String s : strings[i].split("\\b")) {
        if (temp == 0) {
          temp++;
          continue;
        }
        if (!s.trim().isEmpty()) {
          if (temp == 1) {
            size += disposeUnit(s);
            temp++;
          } else {
            used += disposeUnit(s);
            temp = 0;
          }
        }
      }
    }
    String s = new StringBuilder().append("大小 ").append(size).append("G , 已使用").append(used).append("G ,空闲").append(size - used).append("G").toString();
    return s;
  }

  /**
   * 处理单位转换
   * K/KB/M/T 最终转换为G 处理
   *
   * @param s 带单位的数据字符串
   * @return 以G 为单位处理后的数值
   */
  private static int disposeUnit(String s) {
    try {
      s = s.toUpperCase();
      String lastIndex = s.substring(s.length() - 1);
      String num = s.substring(0, s.length() - 1);
      int parseInt = Integer.parseInt(num);
      if ("G".equals(lastIndex)) {
        return parseInt;
      } else if ("T".equals(lastIndex)) {
        return parseInt * 1024;
      } else if ("M".equals(lastIndex)) {
        return parseInt / 1024;
      } else if ("K".equals(lastIndex) || "KB".equals(lastIndex)) {
        return parseInt / (1024 * 1024);
      }
    } catch (NumberFormatException e) {
      log.error("# LinuxStateForShellUtil.disposeUnit() # 处理单位转换时，出现了异常。", e);
      return 0;
    }
    return 0;
  }
}

