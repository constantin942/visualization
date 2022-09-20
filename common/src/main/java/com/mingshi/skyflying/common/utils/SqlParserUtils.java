package com.mingshi.skyflying.common.utils;

import com.mingshi.skyflying.common.constant.Const;
import lombok.extern.slf4j.Slf4j;
import net.sf.jsqlparser.JSQLParserException;
import net.sf.jsqlparser.expression.*;
import net.sf.jsqlparser.expression.operators.conditional.AndExpression;
import net.sf.jsqlparser.expression.operators.conditional.OrExpression;
import net.sf.jsqlparser.expression.operators.relational.ComparisonOperator;
import net.sf.jsqlparser.parser.CCJSqlParserManager;
import net.sf.jsqlparser.parser.CCJSqlParserUtil;
import net.sf.jsqlparser.schema.Table;
import net.sf.jsqlparser.statement.Statement;
import net.sf.jsqlparser.statement.alter.Alter;
import net.sf.jsqlparser.statement.create.index.CreateIndex;
import net.sf.jsqlparser.statement.create.table.CreateTable;
import net.sf.jsqlparser.statement.create.view.CreateView;
import net.sf.jsqlparser.statement.delete.Delete;
import net.sf.jsqlparser.statement.drop.Drop;
import net.sf.jsqlparser.statement.execute.Execute;
import net.sf.jsqlparser.statement.insert.Insert;
import net.sf.jsqlparser.statement.merge.Merge;
import net.sf.jsqlparser.statement.replace.Replace;
import net.sf.jsqlparser.statement.select.*;
import net.sf.jsqlparser.statement.truncate.Truncate;
import net.sf.jsqlparser.statement.update.Update;
import net.sf.jsqlparser.statement.upsert.Upsert;
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

  static final Map<Class, String> CLASS_MAP = new HashMap<>();

  static {
    CLASS_MAP.put(Alter.class, Const.SQL_TYPE_ALTER);
    CLASS_MAP.put(CreateIndex.class, Const.SQL_TYPE_CREATEINDEX);
    CLASS_MAP.put(CreateTable.class, Const.SQL_TYPE_CREATETABLE);
    CLASS_MAP.put(CreateView.class, Const.SQL_TYPE_CREATEVIEW);
    CLASS_MAP.put(Delete.class, Const.SQL_TYPE_DELETE);
    CLASS_MAP.put(Drop.class, Const.SQL_TYPE_DROP);
    CLASS_MAP.put(Execute.class, Const.SQL_TYPE_EXECUTE);
    CLASS_MAP.put(Insert.class, Const.SQL_TYPE_INSERT);
    CLASS_MAP.put(Merge.class, Const.SQL_TYPE_MERGE);
    CLASS_MAP.put(Replace.class, Const.SQL_TYPE_REPLACE);
    CLASS_MAP.put(Select.class, Const.SQL_TYPE_SELECT);
    CLASS_MAP.put(Truncate.class, Const.SQL_TYPE_TRUNCATE);
    CLASS_MAP.put(Update.class, Const.SQL_TYPE_UPDATE);
    CLASS_MAP.put(Upsert.class, Const.SQL_TYPE_UPSERT);
  }

  /**
   * 由于jsqlparser没有获取SQL类型的原始工具，并且在下面操作时需要知道SQL类型，所以编写此工具方法
   *
   * @param sql sql语句
   * @return sql类型，
   * @throws JSQLParserException
   */
  public static String getSqlType(String sql) throws JSQLParserException {
    Statement sqlStmt = CCJSqlParserUtil.parse(new StringReader(sql));

    String sqlType = null;
    try {
      sqlType = CLASS_MAP.get(sqlStmt.getClass());
    } catch (Exception e) {
      log.error("# SqlParserTool.getSqlType() # 根据SQL = 【{}】获取sql类型时，出现了异常。", e);
      return Const.SQL_TYPE_NONE.trim();
    }
    if(null != sqlType){
      return sqlType.trim();
    }
    return null;
  }

  /**
   * 获取join层级
   *
   * @param selectBody
   * @return
   */
  public static List<Join> getJoins(SelectBody selectBody) {
    if (selectBody instanceof PlainSelect) {
      List<Join> joins = ((PlainSelect) selectBody).getJoins();
      return joins;
    }
    return new ArrayList<Join>();
  }

  /**
   * @param selectBody
   * @return
   */
  public static List<Table> getIntoTables(SelectBody selectBody) {
    if (selectBody instanceof PlainSelect) {
      List<Table> tables = ((PlainSelect) selectBody).getIntoTables();
      return tables;
    }
    return new ArrayList<Table>();
  }

  /**
   * @param selectBody
   * @return
   */
  public static void setIntoTables(SelectBody selectBody, List<Table> tables) {
    if (selectBody instanceof PlainSelect) {
      ((PlainSelect) selectBody).setIntoTables(tables);
    }
  }

  /**
   * 获取limit值
   *
   * @param selectBody
   * @return
   */
  public static Limit getLimit(SelectBody selectBody) {
    if (selectBody instanceof PlainSelect) {
      Limit limit = ((PlainSelect) selectBody).getLimit();
      return limit;
    }
    return null;
  }

  /**
   * 为SQL增加limit值
   *
   * @param selectBody
   * @param l
   */
  public static void setLimit(SelectBody selectBody, long l) {
    if (selectBody instanceof PlainSelect) {
      Limit limit = new Limit();
      limit.setRowCount(new LongValue(String.valueOf(l)));
      ((PlainSelect) selectBody).setLimit(limit);
    }
  }

  /**
   * 获取查询字段
   *
   * @param selectBody
   * @return
   */
  public static List<SelectItem> getSelectItems(SelectBody selectBody) {
    if (selectBody instanceof PlainSelect) {
      List<SelectItem> selectItems = ((PlainSelect) selectBody).getSelectItems();
      return selectItems;
    }
    return null;
  }

  /**
   * @Description: 查询sql字段
   * @Author: ywj
   * @Param: [sql]
   * @Return: java.util.List<java.lang.String>
   * @Date: 2020/12/25 15:03
   **/
  public static List<String> selectItems(String sql)
    throws JSQLParserException {
    CCJSqlParserManager parserManager = new CCJSqlParserManager();
    Select select = (Select) parserManager.parse(new StringReader(sql));
    PlainSelect plain = (PlainSelect) select.getSelectBody();
    List<SelectItem> selectitems = plain.getSelectItems();
    List<String> strItems = new ArrayList<>();
    if (selectitems != null) {
      for (SelectItem selectitem : selectitems) {
        strItems.add(selectitem.toString());
      }
    }
    return strItems;
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
      Statement statement = null;
      String newSql = null;
      if (sql.contains(Const.LIKE)) {
        newSql = sql.replace("\"%\"", "");
        statement = CCJSqlParserUtil.parse(newSql);
      } else {
        statement = CCJSqlParserUtil.parse(sql);
      }
      selectStatement = (Select) statement;
      tablesNamesFinder = new TablesNamesFinder();
      tableList = tablesNamesFinder.getTableList(selectStatement);
    } catch (Exception e) {
      // ignore;
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
      log.error("通过sql语句，解析表名时，出现了异常。sql = {}.", sql, e);
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
      log.error("通过sql语句，解析表名时，出现了异常。sql = {}.", sql, e);
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
  public static List<String> selectJoin(String sql)
    throws JSQLParserException {
    Statement statement = CCJSqlParserUtil.parse(sql);
    Select selectStatement = (Select) statement;
    PlainSelect plain = (PlainSelect) selectStatement.getSelectBody();
    List<Join> joinList = plain.getJoins();
    List<String> tablewithjoin = new ArrayList<String>();
    if (joinList != null) {
      for (Join join : joinList) {
        join.setLeft(true);
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
  public static String selectWhere(String sql)
    throws JSQLParserException {
    CCJSqlParserManager parserManager = new CCJSqlParserManager();
    Select select = (Select) parserManager.parse(new StringReader(sql));
    PlainSelect plain = (PlainSelect) select.getSelectBody();
    return plain.getWhere().toString();
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
          Map<String, Object> map = new HashMap<>(Const.NUMBER_EIGHT);
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
    Map<Object, Object> map = new HashMap<>(Const.NUMBER_EIGHT);
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
      if (CollectionUtils.isEmpty(map) && (where.toUpperCase().contains(Const.IS_NOT_NULL) || where.toUpperCase().contains(Const.IS_NULL2))) {
        map.put(Const.LEFT_EXPRESSION, where.substring(0, where.lastIndexOf(Const.IS)));
        map.put(Const.OPERATE, null);
        map.put(Const.RIGHT_EXPRESSION, where.substring(where.lastIndexOf(Const.IS), where.length()));
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
  // public static List<String> selectGroupby(String sql)
  //   throws JSQLParserException {
  //   CCJSqlParserManager parserManager = new CCJSqlParserManager();
  //   Select select = (Select) parserManager.parse(new StringReader(sql));
  //   PlainSelect plain = (PlainSelect) select.getSelectBody();
  //   List<Expression> groupByColumnReferences = plain.getGroupBy().getGroupByExpressions();
  //   List<String> strGroupby = new ArrayList<String>();
  //   if (null != groupByColumnReferences) {
  //     for (Expression groupByColumnReference : groupByColumnReferences) {
  //       strGroupby.add(groupByColumnReference.toString());
  //     }
  //   }
  //   return strGroupby;
  // }

  /**
   * @Description: 查询order by
   * @Author: ywj
   * @Param: [sql]
   * @Return: java.util.List<java.lang.String>
   * @Date: 2020/12/25 15:13
   **/
  public static List<String> selectOrderby(String sql)
    throws JSQLParserException {
    CCJSqlParserManager parserManager = new CCJSqlParserManager();
    Select select = (Select) parserManager.parse(new StringReader(sql));
    PlainSelect plain = (PlainSelect) select.getSelectBody();
    List<OrderByElement> orderByElements = plain.getOrderByElements();
    List<String> strOrderby = new ArrayList<String>();
    if (orderByElements != null) {
      for (OrderByElement orderByElement : orderByElements) {
        strOrderby.add(orderByElement.toString());
      }
    }
    return strOrderby;
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
