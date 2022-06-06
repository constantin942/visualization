package com.mingshi.skyflying.utils;

import lombok.extern.slf4j.Slf4j;
import net.sf.jsqlparser.JSQLParserException;
import net.sf.jsqlparser.expression.BinaryExpression;
import net.sf.jsqlparser.expression.Expression;
import net.sf.jsqlparser.expression.ExpressionVisitorAdapter;
import net.sf.jsqlparser.expression.Parenthesis;
import net.sf.jsqlparser.expression.operators.conditional.AndExpression;
import net.sf.jsqlparser.expression.operators.conditional.OrExpression;
import net.sf.jsqlparser.expression.operators.relational.ComparisonOperator;
import net.sf.jsqlparser.parser.CCJSqlParserManager;
import net.sf.jsqlparser.parser.CCJSqlParserUtil;
import net.sf.jsqlparser.statement.Statement;
import net.sf.jsqlparser.statement.delete.Delete;
import net.sf.jsqlparser.statement.insert.Insert;
import net.sf.jsqlparser.statement.select.*;
import net.sf.jsqlparser.statement.update.Update;
import net.sf.jsqlparser.util.TablesNamesFinder;
import net.sf.jsqlparser.util.deparser.ExpressionDeParser;
import org.springframework.util.CollectionUtils;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * <p>Title:SqlParserUtils</p>
 *
 * @description: sql语句解析工具
 * @author: ywj
 * @create: 2020-12-25 15:02
 */
@Slf4j
public class SqlParserUtils {
  /**
   * @Description: 查询sql字段
   * @Author: ywj
   * @Param: [sql]
   * @Return: java.util.List<java.lang.String>
   * @Date: 2020/12/25 15:03
   **/
  public static List<String> select_items(String sql)
    throws JSQLParserException {
    CCJSqlParserManager parserManager = new CCJSqlParserManager();
    Select select = (Select) parserManager.parse(new StringReader(sql));
    PlainSelect plain = (PlainSelect) select.getSelectBody();
    List<SelectItem> selectitems = plain.getSelectItems();
    List<String> str_items = new ArrayList<String>();
    if (selectitems != null) {
      for (SelectItem selectitem : selectitems) {
        str_items.add(selectitem.toString());
      }
    }
    return str_items;
  }

  /**
   * @Description: 查询表名table
   * @Author: ywj
   * @Param: [sql]
   * @Return: java.util.List<java.lang.String>
   * @Date: 2020/12/25 15:04
   **/
  public static List<String> selectTable(String sql) {
    Select selectStatement = null;
    List<String> tableList = new ArrayList<>();
    TablesNamesFinder tablesNamesFinder = null;
    try {
      Statement statement = CCJSqlParserUtil.parse(sql);
      selectStatement = (Select) statement;
      tablesNamesFinder = new TablesNamesFinder();
      tableList = tablesNamesFinder.getTableList(selectStatement);
    } catch (Exception e) {
      // TODO：当这里出现异常的时候，大部分是因为SQL语句不完整造成的。有空了需要确认下，是不是skywalking探针获取到的SQL就是这个样子的。2022-06-06 16:47:37
      log.error("通过sql语句，解析表名时，出现了异常。sql = {}.", sql);
    }
    return tableList;
  }

  public static List<String> insertTable(String sql) {
    Insert insertStatement = null;
    List<String> tableList = new ArrayList<>();
    TablesNamesFinder tablesNamesFinder = null;
    try {
      Statement statement = CCJSqlParserUtil.parse(sql);
      insertStatement = (Insert) statement;
      tablesNamesFinder = new TablesNamesFinder();
      tableList = tablesNamesFinder.getTableList(insertStatement);
    } catch (JSQLParserException e) {
      log.error("通过sql语句，解析表名时，出现了异常。sql = {}.", sql);
    }
    return tableList;
  }

  public static List<String> updateTable(String sql) {
    Update updateStatement = null;
    List<String> tableList = new ArrayList<>();
    TablesNamesFinder tablesNamesFinder = null;
    try {
      Statement statement = CCJSqlParserUtil.parse(sql);
      updateStatement = (Update) statement;
      tablesNamesFinder = new TablesNamesFinder();
      tableList = tablesNamesFinder.getTableList(updateStatement);
    } catch (Exception e) {
      log.error("通过sql语句，解析表名时，出现了异常。sql = {}.", sql, e);
    }
    return tableList;
  }

  public static List<String> deleteTable(String sql) {
    Delete deleteStatement = null;
    List<String> tableList = new ArrayList<>();
    TablesNamesFinder tablesNamesFinder = null;
    try {
      Statement statement = CCJSqlParserUtil.parse(sql);
      deleteStatement = (Delete) statement;
      tablesNamesFinder = new TablesNamesFinder();
      tableList = tablesNamesFinder.getTableList(deleteStatement);
    } catch (Exception e) {
      log.error("通过sql语句，解析表名时，出现了异常。sql = {}.", sql);
    }
    return tableList;
  }

  /**
   * @Description: 查询join
   * @Author: ywj
   * @Param: [sql]
   * @Return: java.util.List<java.lang.String>
   * @Date: 2020/12/25 15:05
   **/
  public static List<String> select_join(String sql)
    throws JSQLParserException {
    Statement statement = CCJSqlParserUtil.parse(sql);
    Select selectStatement = (Select) statement;
    PlainSelect plain = (PlainSelect) selectStatement.getSelectBody();
    List<Join> joinList = plain.getJoins();
    List<String> tablewithjoin = new ArrayList<String>();
    if (joinList != null) {
      for (Join join : joinList) {
        join.setLeft(true);//是否开放left jion中的left
        tablewithjoin.add(join.toString());
      }
    }
    return tablewithjoin;
  }

  /**
   * @Description: 查询where
   * @Author: ywj
   * @Param: [sql]
   * @Return: java.lang.String
   * @Date: 2020/12/25 15:06
   **/
  public static String select_where(String sql)
    throws JSQLParserException {
    CCJSqlParserManager parserManager = new CCJSqlParserManager();
    Select select = (Select) parserManager.parse(new StringReader(sql));
    PlainSelect plain = (PlainSelect) select.getSelectBody();
    Expression where_expression = plain.getWhere();
    return where_expression.toString();
  }

  /**
   * @Description: 对where条件解析并返回结果
   * @Author: ywj
   * @Param: [sql, metadata:是否开启原数据]
   * @Return: java.util.List<java.lang.Object>
   * @Date: 2020/12/28 17:14
   **/
  public static List<Map<String, Object>> parseWhere(String sql) {
    try {
      Select select = (Select) CCJSqlParserUtil.parse(sql);
      SelectBody selectBody = select.getSelectBody();
      PlainSelect plainSelect = (PlainSelect) selectBody;
      Expression expr = CCJSqlParserUtil.parseCondExpression(plainSelect.getWhere().toString());
      List<Map<String, Object>> arrList = new ArrayList<>();
      expr.accept(new ExpressionDeParser() {
        int depth = 0;

        @Override
        public void visit(Parenthesis parenthesis) {
          depth++;
          parenthesis.getExpression().accept(this);
          depth--;
        }

        @Override
        public void visit(OrExpression orExpression) {
          visitBinaryExpr(orExpression, "OR");
        }

        @Override
        public void visit(AndExpression andExpression) {
          visitBinaryExpr(andExpression, "AND");
        }

        private void visitBinaryExpr(BinaryExpression expr, String operator) {
          Map<String, Object> map = new HashMap<>();
          if (!(expr.getLeftExpression() instanceof OrExpression)
            && !(expr.getLeftExpression() instanceof AndExpression)
            && !(expr.getLeftExpression() instanceof Parenthesis)) {
            getBuffer();
          }
          expr.getLeftExpression().accept(this);
          map.put("leftExpression", expr.getLeftExpression());
          map.put("operator", operator);
          if (!(expr.getRightExpression() instanceof OrExpression)
            && !(expr.getRightExpression() instanceof AndExpression)
            && !(expr.getRightExpression() instanceof Parenthesis)) {
            getBuffer();
          }
          expr.getRightExpression().accept(this);
          map.put("rightExpression", expr.getRightExpression());
          arrList.add(map);
        }
      });
      return arrList;
    } catch (JSQLParserException e) {
      e.printStackTrace();
    }
    return null;
  }

  /**
   * @Description: 完全解析where单个条件并返回
   * @Author: ywj
   * @Param: [where]
   * @Return: java.util.Map<java.lang.Object, java.lang.Object>
   * @Date: 2020/12/28 16:47
   **/
  public static Map<Object, Object> fullResolutionWhere(String where) {
    Map<Object, Object> map = new HashMap<>();
    try {
      Expression expr = CCJSqlParserUtil.parseCondExpression(where);
      expr.accept(new ExpressionVisitorAdapter() {
        @Override
        protected void visitBinaryExpression(BinaryExpression expr) {
          if (expr instanceof ComparisonOperator) {
            map.put("leftExpression", expr.getLeftExpression());
            map.put("operate", expr.getStringExpression());
            map.put("rightExpression", expr.getRightExpression());
          }
          super.visitBinaryExpression(expr);
        }
      });
      //暂时无法解析IS NOT NULL 和 IS NULL
      if (CollectionUtils.isEmpty(map) && (where.toUpperCase().contains("IS NOT NULL") || where.toUpperCase().contains("IS NULL"))) {
        map.put("leftExpression", where.substring(0, where.lastIndexOf("IS")));
        map.put("operate", null);
        map.put("rightExpression", where.substring(where.lastIndexOf("IS"), where.length()));
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
    return map;
  }


  /**
   * @Description: 查询 group by
   * @Author: ywj
   * @Param: [sql]
   * @Return: java.util.List<java.lang.String>
   * @Date: 2020/12/25 15:10
   **/
  public static List<String> select_groupby(String sql)
    throws JSQLParserException {
    CCJSqlParserManager parserManager = new CCJSqlParserManager();
    Select select = (Select) parserManager.parse(new StringReader(sql));
    PlainSelect plain = (PlainSelect) select.getSelectBody();
    List<Expression> GroupByColumnReferences = plain.getGroupBy().getGroupByExpressions();
    List<String> str_groupby = new ArrayList<String>();
    if (GroupByColumnReferences != null) {
      for (Expression groupByColumnReference : GroupByColumnReferences) {
        str_groupby.add(groupByColumnReference.toString());
      }
    }
    return str_groupby;
  }

  /**
   * @Description: 查询order by
   * @Author: ywj
   * @Param: [sql]
   * @Return: java.util.List<java.lang.String>
   * @Date: 2020/12/25 15:13
   **/
  public static List<String> select_orderby(String sql)
    throws JSQLParserException {
    CCJSqlParserManager parserManager = new CCJSqlParserManager();
    Select select = (Select) parserManager.parse(new StringReader(sql));
    PlainSelect plain = (PlainSelect) select.getSelectBody();
    List<OrderByElement> OrderByElements = plain.getOrderByElements();
    List<String> str_orderby = new ArrayList<String>();
    if (OrderByElements != null) {
      for (OrderByElement orderByElement : OrderByElements) {
        str_orderby.add(orderByElement.toString());
      }
    }
    return str_orderby;
  }

  /**
   * @Description: 判断是否为多级子查询
   * @Author: ywj
   * @Param: [selectBody]
   * @Return: boolean
   * @Date: 2020/12/29 15:28
   * @Demo: select * from (select userid from (select userid from a)a) a
   **/
  public static boolean isMultiSubSelect(SelectBody selectBody) {
    if (selectBody instanceof PlainSelect) {
      FromItem fromItem = ((PlainSelect) selectBody).getFromItem();
      if (fromItem instanceof SubSelect) {
        SelectBody subBody = ((SubSelect) fromItem).getSelectBody();
        if (subBody instanceof PlainSelect) {
          FromItem subFromItem = ((PlainSelect) subBody).getFromItem();
          if (subFromItem instanceof SubSelect) {
            return true;
          }
        }
      }
    }
    return false;
  }


}
