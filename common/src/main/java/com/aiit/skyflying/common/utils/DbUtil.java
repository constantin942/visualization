package com.aiit.skyflying.common.utils;

import com.aiit.skyflying.common.constant.Const;
import org.apache.poi.xwpf.usermodel.*;
import org.apache.tomcat.util.http.fileupload.FileUtils;
import org.openxmlformats.schemas.wordprocessingml.x2006.main.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

import java.io.File;
import java.io.FileOutputStream;
import java.math.BigInteger;
import java.util.*;

/**
 * <B>主类名称: DbUtil</B>
 * <B>概要说明：生成数据库设计文档的功能</B>
 * Author zm
 * Date 2022/8/23 14:14
 *
 * @Version 1.0
 **/
@Component
public class DbUtil {
  @Autowired
  private JdbcTemplate jdbcTemplate;

  /**
   * 生成数据库设计文档
   */
  public void createWord(String dataName,String docDirPath) {
    XWPFDocument xdoc = new XWPFDocument();
    XWPFParagraph title = xdoc.createParagraph();
    title.setAlignment(ParagraphAlignment.CENTER);
    XWPFRun rt = title.createRun();
    rt.setBold(true);
    rt.setFontFamily("微软雅黑");
    rt.setText(dataName + "数据库设计文档");
    rt.setFontSize(20);
    rt.setColor("333333");
    rt.setBold(true);

    Map<String, String[][]> datas = dataInfo(dataName);
    Set<String> keySet = datas.keySet();
    for (String table : keySet) {
      XWPFParagraph headLine1 = xdoc.createParagraph();
      headLine1.setAlignment(ParagraphAlignment.LEFT);
      XWPFRun runHeadLine1 = headLine1.createRun();
      runHeadLine1.setText(table);
      runHeadLine1.setFontSize(14);
      runHeadLine1.setFontFamily("微软雅黑");
      runHeadLine1.setColor("a6a6a6");

      String[][] clumns = datas.get(table);

      XWPFTable dTable = xdoc.createTable(clumns.length + 1, 3);
      createTable(dTable, xdoc, clumns);
      setEmptyRow(xdoc, rt);
    }
    // 在服务器端生成
    FileOutputStream fos = null;
    try {
      String docPath = docDirPath + File.separator + dataName + "_" + (new Date()).getTime() + ".docx";
      FileUtils.forceMkdirParent(new File(docPath));
      fos = new FileOutputStream(docPath);
      xdoc.write(fos);
      fos.close();
    } catch (Exception e) {
      e.printStackTrace();
    }

  }

  /**
   * 获取数据库每个表的信息
   *
   * @param data
   * @return
   */
  public Map<String, String[][]> dataInfo(String data) {
    List<Map<String, Object>> list = jdbcTemplate.queryForList(
      "select table_name,table_comment from information_schema.tables where table_schema = ?", data);
    Map<String, String[][]> datas = new HashMap<>(Const.INITAL_SIZE);
    for (Map<String, Object> map : list) {
      String tableName = map.get(Const.TABLE_NAME2) + "";
      String tableComment = map.get(Const.TABLE_COMMENT) + "";
      datas.put("表:" + tableName + ":" + tableComment, tableInfo(data + "." + tableName));
    }
    return datas;
  }

  /**
   * 获取每个表的字段信息
   *
   * @param table
   * @return
   */
  public String[][] tableInfo(String table) {
    List<Map<String, Object>> list = jdbcTemplate.queryForList("SHOW FULL FIELDS FROM " + table);
    String[][] tables = new String[list.size()][3];
    for (int i = 0; i < list.size(); i++) {
      Map<String, Object> map = list.get(i);
      String[] info = new String[3];
      info[0] = map.get("Field") + "";
      info[1] = map.get("Type") + "";
      info[2] = map.get("Comment") + "";
      tables[i] = info;
    }
    return tables;
  }

  /**
   * 生成表格
   *
   * @param xTable
   * @param xdoc
   */
  public static void createTable(XWPFTable xTable, XWPFDocument xdoc, String[][] clumns) {
    String bgColor = "111111";
    CTTbl ttbl = xTable.getCTTbl();
    CTTblPr tblPr = ttbl.getTblPr() == null ? ttbl.addNewTblPr() : ttbl.getTblPr();
    CTTblWidth tblWidth = tblPr.isSetTblW() ? tblPr.getTblW() : tblPr.addNewTblW();
    tblWidth.setW(new BigInteger("8600"));
    tblWidth.setType(STTblWidth.DXA);
    setCellText(xdoc, getCellHight(xTable, 0, 0), "字段名", bgColor, 1000);
    setCellText(xdoc, getCellHight(xTable, 0, 1), "类型", bgColor, 3800);
    setCellText(xdoc, getCellHight(xTable, 0, 2), "说明", bgColor, 3800);
    int length = clumns.length;
    for (int i = 0; i < length; i++) {
      setCellText(xdoc, getCellHight(xTable, i + 1, 0), clumns[i][0], bgColor, 1000);
      setCellText(xdoc, getCellHight(xTable, i + 1, 1), clumns[i][1], bgColor, 3800);
      setCellText(xdoc, getCellHight(xTable, i + 1, 2), clumns[i][2], bgColor, 3800);
    }
  }

  /**
   * 设置表格高度
   * @param xTable
   * @param rowNomber
   * @param cellNumber
   * @return
   */
  private static XWPFTableCell getCellHight(XWPFTable xTable, int rowNomber, int cellNumber) {
    XWPFTableRow row = null;
    row = xTable.getRow(rowNomber);
    row.setHeight(100);
    XWPFTableCell cell = null;
    cell = row.getCell(cellNumber);
    return cell;
  }

  /**
   * 单元格设置文本
   *
   * @param xDocument
   * @param cell
   * @param text
   * @param bgcolor
   * @param width
   */
  private static void setCellText(XWPFDocument xDocument, XWPFTableCell cell, String text, String bgcolor, int width) {
    CTTc cttc = cell.getCTTc();
    CTTcPr cellPr = cttc.addNewTcPr();
    cellPr.addNewTcW().setW(BigInteger.valueOf(width));
    XWPFParagraph pio = cell.addParagraph();
    cell.removeParagraph(0);
    XWPFRun rio = pio.createRun();
    rio.setFontFamily("微软雅黑");
    rio.setColor("000000");
    rio.setFontSize(12);
    rio.setText(text);
  }

  /**
   * 设置表格间的空行
   * @param xdoc
   * @param r1
   */
  public static void setEmptyRow(XWPFDocument xdoc, XWPFRun r1) {
    XWPFParagraph p1 = xdoc.createParagraph();
    p1.setAlignment(ParagraphAlignment.CENTER);
    p1.setVerticalAlignment(TextAlignment.CENTER);
    r1 = p1.createRun();
  }
}
